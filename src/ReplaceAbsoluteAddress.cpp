#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <vector>

#include "clang/AST/AST.h"
#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendActions.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Refactoring.h"
#include "clang/Tooling/Tooling.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

#define DEBUG_TYPE "abs-addr-replace"
//===----------------------------------------------------------------------===//
// Command-line Options
//===----------------------------------------------------------------------===//

static llvm::cl::OptionCategory MyToolCategory(
    "replace-absolute-address options");

static llvm::cl::opt<std::string> OutputFilename(
    "o", llvm::cl::desc("Specify output filename"),
    llvm::cl::value_desc("filename"), llvm::cl::init(""));

//===----------------------------------------------------------------------===//
// ReplacementCollector
//===----------------------------------------------------------------------===//

class ReplacementCollector : public MatchFinder::MatchCallback {
 public:
  ReplacementCollector(Replacements &Reps,
                       std::map<uint64_t, std::string> &GlobalMap,
                       std::vector<std::string> &GlobalDecls)
      : Reps(Reps), GlobalMap(GlobalMap), GlobalDecls(GlobalDecls) {}

  virtual void run(const MatchFinder::MatchResult &Result) override {
    LLVM_DEBUG(llvm::dbgs() << "[ReplacementCollector] run() called\n");
    const auto *CastExpr = Result.Nodes.getNodeAs<CStyleCastExpr>("absAddr");
    if (!CastExpr) {
      LLVM_DEBUG(llvm::dbgs()
                 << "[ReplacementCollector] No cast expression found.\n");
      return;
    }

    SourceManager &SM = *Result.SourceManager;
    LangOptions LangOpts = Result.Context->getLangOpts();

    SourceLocation Begin = SM.getExpansionLoc(CastExpr->getBeginLoc());
    SourceLocation EndToken = SM.getExpansionLoc(CastExpr->getEndLoc());
    LLVM_DEBUG(llvm::dbgs()
               << "[ReplacementCollector] Expansion Begin: "
               << Begin.printToString(SM) << ", Expansion EndToken: "
               << EndToken.printToString(SM) << "\n");
    SourceLocation End = Lexer::getLocForEndOfToken(EndToken, 0, SM, LangOpts);
    if (Begin.isInvalid() || End.isInvalid()) {
      LLVM_DEBUG(llvm::dbgs()
                 << "[ReplacementCollector] Invalid Begin or End location.\n");
      return;
    }
    CharSourceRange CharRange = CharSourceRange::getCharRange(Begin, End);
    LLVM_DEBUG(llvm::dbgs()
               << "[ReplacementCollector] Computed expansion range: "
               << Begin.printToString(SM) << " to " << End.printToString(SM)
               << "\n");

    StringRef OrigText = Lexer::getSourceText(CharRange, SM, LangOpts);
    LLVM_DEBUG(llvm::dbgs() << "[ReplacementCollector] Original cast text: '"
                            << OrigText << "'\n");

    const Expr *SubExpr = CastExpr->getSubExprAsWritten();
    if (const auto *IntLit =
            dyn_cast<IntegerLiteral>(SubExpr->IgnoreParenImpCasts())) {
      uint64_t value = IntLit->getValue().getLimitedValue();
      LLVM_DEBUG(llvm::dbgs() << "[ReplacementCollector] IntegerLiteral value: "
                              << value << "\n");
      std::string globalName;

      if (GlobalMap.find(value) == GlobalMap.end()) {
        globalName = "glob_" + std::to_string(GlobalMap.size());
        GlobalMap[value] = globalName;
        LLVM_DEBUG(llvm::dbgs()
                   << "[ReplacementCollector] Generated new global name: "
                   << globalName << "\n");

        QualType castType = CastExpr->getType();
        if (castType->isPointerType()) {
          QualType pointeeType = castType->getPointeeType();
          std::string Decl;
          if (pointeeType->isVoidType()) {
            Decl = "unsigned char " + globalName + "[1024];\n";
          } else {
            std::string typeStr = pointeeType.getAsString();
            if (castType.isVolatileQualified() ||
                pointeeType.isVolatileQualified())
              typeStr = "volatile " + typeStr;
            Decl = typeStr + " " + globalName + ";\n";
          }
          GlobalDecls.push_back(Decl);
          LLVM_DEBUG(llvm::dbgs()
                     << "[ReplacementCollector] Global declaration: " << Decl);
        }
      } else {
        globalName = GlobalMap[value];
        LLVM_DEBUG(llvm::dbgs()
                   << "[ReplacementCollector] Using existing global name: "
                   << globalName << "\n");
      }

      Replacement Rep(SM, CharRange, globalName);
      LLVM_DEBUG(llvm::dbgs()
                 << "[ReplacementCollector] Creating replacement for "
                    "expansion range (offset "
                 << Rep.getOffset() << ", length " << Rep.getLength()
                 << ") with text: " << globalName << "\n");
      if (llvm::Error Err = Reps.add(Rep)) {
        LLVM_DEBUG(llvm::dbgs()
                   << "[ReplacementCollector] Error adding replacement: "
                   << llvm::toString(std::move(Err)) << "\n");
      } else {
        LLVM_DEBUG(
            llvm::dbgs()
            << "[ReplacementCollector] Replacement added successfully.\n");
      }
    } else {
      LLVM_DEBUG(llvm::dbgs() << "[ReplacementCollector] Subexpression is not "
                                 "an IntegerLiteral.\n");
    }
  }

 private:
  Replacements &Reps;
  std::map<uint64_t, std::string> &GlobalMap;
  std::vector<std::string> &GlobalDecls;
};

//===----------------------------------------------------------------------===//
// ReplacementASTConsumer
//===----------------------------------------------------------------------===//

class ReplacementASTConsumer : public ASTConsumer {
 public:
  ReplacementASTConsumer(Replacements &Reps)
      : Collector(Reps, GlobalMap, GlobalDecls) {
    LLVM_DEBUG(llvm::dbgs()
               << "[ASTConsumer] Adding matcher for C-style cast with "
                  "IntegerLiteral.\n");
    Matcher.addMatcher(
        cStyleCastExpr(hasSourceExpression(integerLiteral())).bind("absAddr"),
        &Collector);
  }

  virtual void HandleTranslationUnit(ASTContext &Context) override {
    LLVM_DEBUG(llvm::dbgs() << "[ASTConsumer] Handling Translation Unit.\n");
    Matcher.matchAST(Context);
    LLVM_DEBUG(llvm::dbgs()
               << "[ASTConsumer] Translation Unit handled. Collected "
               << GlobalDecls.size() << " global declarations.\n");
  }

  std::vector<std::string> getGlobalDecls() const { return GlobalDecls; }

 private:
  ReplacementCollector Collector;
  std::map<uint64_t, std::string> GlobalMap;
  std::vector<std::string> GlobalDecls;
  MatchFinder Matcher;
};

//===----------------------------------------------------------------------===//
// ReplacementFrontendAction
//===----------------------------------------------------------------------===//

class ReplacementFrontendAction : public ASTFrontendAction {
 public:
  ReplacementFrontendAction() : ConsumerPtr(nullptr) {}

  void EndSourceFileAction() override {
    LLVM_DEBUG(llvm::dbgs()
               << "[FrontendAction] EndSourceFileAction called.\n");
    SourceManager &SM = getCompilerInstance().getSourceManager();
    FileID MainFID = SM.getMainFileID();
    LLVM_DEBUG(llvm::dbgs() << "[FrontendAction] Main FileID: "
                            << MainFID.getHashValue() << "\n");

    StringRef OriginalText = SM.getBufferData(MainFID);
    LLVM_DEBUG(llvm::dbgs() << "[FrontendAction] Original text length: "
                            << OriginalText.size() << "\n");

    size_t insertionOffset = 0;
    size_t lastIncludePos = OriginalText.rfind("#include");
    if (lastIncludePos != StringRef::npos) {
      size_t newlinePos = OriginalText.find('\n', lastIncludePos);
      if (newlinePos != StringRef::npos) {
        insertionOffset = newlinePos + 1;
        LLVM_DEBUG(llvm::dbgs()
                   << "[FrontendAction] Inserting global declarations after "
                      "last include at offset "
                   << insertionOffset << ".\n");
      } else {
        LLVM_DEBUG(llvm::dbgs()
                   << "[FrontendAction] Newline not found after last "
                      "#include. Inserting at end of file.\n");
        insertionOffset = OriginalText.size();
      }
    } else {
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] No #include found. Inserting at "
                    "beginning of file.\n");
      insertionOffset = 0;
    }

    std::vector<std::string> GlobalDecls;
    if (ConsumerPtr) {
      GlobalDecls = ConsumerPtr->getGlobalDecls();
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Retrieved " << GlobalDecls.size()
                 << " global declarations.\n");
    } else {
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] ERROR: ConsumerPtr is null!\n");
      return;
    }

    std::string GlobalText;
    for (const auto &decl : GlobalDecls) {
      GlobalText += decl;
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Global declaration: " << decl);
    }

    SourceLocation FileStart = SM.getLocForStartOfFile(MainFID);
    SourceLocation insertionLoc = FileStart.getLocWithOffset(insertionOffset);
    LLVM_DEBUG(llvm::dbgs()
               << "[FrontendAction] Insertion location computed at offset "
               << insertionOffset << ": " << insertionLoc.printToString(SM)
               << "\n");

    Replacement GlobalInsert(SM, insertionLoc, 0, GlobalText);
    if (llvm::Error Err = Reps.add(GlobalInsert))
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Error adding global decl replacement: "
                 << llvm::toString(std::move(Err)) << "\n");
    else
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Global decl replacement added.\n");

    LLVM_DEBUG(llvm::dbgs() << "[FrontendAction] Total replacements: "
                            << Reps.size() << "\n");
    for (const auto &R : Reps) {
      LLVM_DEBUG(llvm::dbgs() << "[FrontendAction] Replacement: offset="
                              << R.getOffset() << ", length=" << R.getLength()
                              << " => '" << R.getReplacementText() << "'\n");
    }

    llvm::Expected<std::string> ChangedText =
        applyAllReplacements(OriginalText, Reps);
    if (!ChangedText) {
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Error applying replacements: "
                 << llvm::toString(ChangedText.takeError()) << "\n");
      return;
    }
    LLVM_DEBUG(llvm::dbgs()
               << "[FrontendAction] Replacements applied successfully. Final "
                  "text length: "
               << ChangedText->size() << "\n");

    if (OutputFilename.empty()) {
      llvm::outs() << *ChangedText;
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Final output written to stdout.\n");
    } else {
      std::error_code EC;
      llvm::raw_fd_ostream outFile(OutputFilename, EC, llvm::sys::fs::OF_None);
      if (EC) {
        LLVM_DEBUG(llvm::dbgs()
                   << "[FrontendAction] Error opening output file: "
                   << EC.message() << "\n");
        return;
      }
      outFile << *ChangedText;
      LLVM_DEBUG(llvm::dbgs()
                 << "[FrontendAction] Final output written to file: "
                 << OutputFilename << "\n");
    }
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    LLVM_DEBUG(llvm::dbgs()
               << "[FrontendAction] Creating ASTConsumer for file: " << InFile
               << "\n");
    Reps.clear();
    auto ConsumerUniquePtr = std::make_unique<ReplacementASTConsumer>(Reps);
    ConsumerPtr = ConsumerUniquePtr.get();
    return ConsumerUniquePtr;
  }

 private:
  Replacements Reps;
  ReplacementASTConsumer *ConsumerPtr;
};

//===----------------------------------------------------------------------===//
// main
//===----------------------------------------------------------------------===//

int main(int argc, const char **argv) {
  LLVM_DEBUG(llvm::dbgs() << "[main] Starting tool with " << argc
                          << " arguments.\n");
  auto OptionsParserExpected =
      CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!OptionsParserExpected) {
    LLVM_DEBUG(llvm::dbgs()
               << "[main] Error creating CommonOptionsParser: "
               << llvm::toString(OptionsParserExpected.takeError()) << "\n");
    return 1;
  }
  CommonOptionsParser OptionsParser = std::move(OptionsParserExpected.get());
  LLVM_DEBUG(llvm::dbgs() << "[main] Parsed options. Number of source paths: "
                          << OptionsParser.getSourcePathList().size() << "\n");
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  LLVM_DEBUG(llvm::dbgs() << "[main] Running frontend action...\n");
  int Result =
      Tool.run(newFrontendActionFactory<ReplacementFrontendAction>().get());
  LLVM_DEBUG(llvm::dbgs() << "[main] Tool run completed with result: " << Result
                          << "\n");
  return Result;
}
