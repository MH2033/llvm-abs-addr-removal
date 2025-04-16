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
#include "llvm/Support/raw_ostream.h"

using namespace clang;
using namespace clang::ast_matchers;
using namespace clang::tooling;

//===----------------------------------------------------------------------===//
// Command-line Options
//===----------------------------------------------------------------------===//

static llvm::cl::OptionCategory MyToolCategory(
    "replace-absolute-address options");

// The -o option: if specified, output will be saved to that filename.
static llvm::cl::opt<std::string> OutputFilename(
    "o", llvm::cl::desc("Specify output filename"),
    llvm::cl::value_desc("filename"), llvm::cl::init(""));

//===----------------------------------------------------------------------===//
// ReplacementCollector
//===----------------------------------------------------------------------===//

// This callback collects Replacements for every matched cast expression.
class ReplacementCollector : public MatchFinder::MatchCallback {
 public:
  ReplacementCollector(Replacements &Reps,
                       std::map<uint64_t, std::string> &GlobalMap,
                       std::vector<std::string> &GlobalDecls)
      : Reps(Reps), GlobalMap(GlobalMap), GlobalDecls(GlobalDecls) {}

  virtual void run(const MatchFinder::MatchResult &Result) override {
    llvm::errs() << "[ReplacementCollector] run() called\n";
    const auto *CastExpr = Result.Nodes.getNodeAs<CStyleCastExpr>("absAddr");
    if (!CastExpr) {
      llvm::errs() << "[ReplacementCollector] No cast expression found.\n";
      return;
    }

    SourceManager &SM = *Result.SourceManager;
    LangOptions LangOpts = Result.Context->getLangOpts();

    // Use expansion locations so that we modify the usage in the main file.
    SourceLocation Begin = SM.getExpansionLoc(CastExpr->getBeginLoc());
    SourceLocation EndToken = SM.getExpansionLoc(CastExpr->getEndLoc());
    llvm::errs() << "[ReplacementCollector] Expansion Begin: "
                 << Begin.printToString(SM)
                 << ", Expansion EndToken: " << EndToken.printToString(SM)
                 << "\n";
    SourceLocation End = Lexer::getLocForEndOfToken(EndToken, 0, SM, LangOpts);
    if (Begin.isInvalid() || End.isInvalid()) {
      llvm::errs() << "[ReplacementCollector] Invalid Begin or End location.\n";
      return;
    }
    CharSourceRange CharRange = CharSourceRange::getCharRange(Begin, End);
    llvm::errs() << "[ReplacementCollector] Computed expansion range: "
                 << Begin.printToString(SM) << " to " << End.printToString(SM)
                 << "\n";

    // Log the original text.
    StringRef OrigText = Lexer::getSourceText(CharRange, SM, LangOpts);
    llvm::errs() << "[ReplacementCollector] Original cast text: '" << OrigText
                 << "'\n";

    // Process the subexpression: if it's an IntegerLiteral, extract its value.
    const Expr *SubExpr = CastExpr->getSubExprAsWritten();
    if (const auto *IntLit =
            dyn_cast<IntegerLiteral>(SubExpr->IgnoreParenImpCasts())) {
      uint64_t value = IntLit->getValue().getLimitedValue();
      llvm::errs() << "[ReplacementCollector] IntegerLiteral value: " << value
                   << "\n";
      std::string globalName;

      // Generate or retrieve a global variable name.
      if (GlobalMap.find(value) == GlobalMap.end()) {
        globalName = "glob_" + std::to_string(GlobalMap.size());
        GlobalMap[value] = globalName;
        llvm::errs() << "[ReplacementCollector] Generated new global name: "
                     << globalName << "\n";

        // Generate a global declaration based on the cast type.
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
          llvm::errs() << "[ReplacementCollector] Global declaration: " << Decl;
        }
      } else {
        globalName = GlobalMap[value];
        llvm::errs() << "[ReplacementCollector] Using existing global name: "
                     << globalName << "\n";
      }

      // Create a Replacement that replaces the entire cast expression with the
      // global variable name.
      Replacement Rep(SM, CharRange, globalName);
      llvm::errs() << "[ReplacementCollector] Creating replacement for "
                      "expansion range (offset "
                   << Rep.getOffset() << ", length " << Rep.getLength()
                   << ") with text: " << globalName << "\n";
      if (llvm::Error Err = Reps.add(Rep)) {
        llvm::errs() << "[ReplacementCollector] Error adding replacement: "
                     << llvm::toString(std::move(Err)) << "\n";
      } else {
        llvm::errs()
            << "[ReplacementCollector] Replacement added successfully.\n";
      }
    } else {
      llvm::errs()
          << "[ReplacementCollector] Subexpression is not an IntegerLiteral.\n";
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
    llvm::errs() << "[ASTConsumer] Adding matcher for C-style cast with "
                    "IntegerLiteral.\n";
    Matcher.addMatcher(
        cStyleCastExpr(hasSourceExpression(integerLiteral())).bind("absAddr"),
        &Collector);
  }

  virtual void HandleTranslationUnit(ASTContext &Context) override {
    llvm::errs() << "[ASTConsumer] Handling Translation Unit.\n";
    Matcher.matchAST(Context);
    llvm::errs() << "[ASTConsumer] Translation Unit handled. Collected "
                 << GlobalDecls.size() << " global declarations.\n";
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

// To fix our earlier segfault (consumer pointer null issue), we store a raw
// pointer.
class ReplacementFrontendAction : public ASTFrontendAction {
 public:
  ReplacementFrontendAction() : ConsumerPtr(nullptr) {}

  void EndSourceFileAction() override {
    llvm::errs() << "[FrontendAction] EndSourceFileAction called.\n";
    SourceManager &SM = getCompilerInstance().getSourceManager();
    FileID MainFID = SM.getMainFileID();
    llvm::errs() << "[FrontendAction] Main FileID: " << MainFID.getHashValue()
                 << "\n";

    // Get original text of the main file.
    StringRef OriginalText = SM.getBufferData(MainFID);
    llvm::errs() << "[FrontendAction] Original text length: "
                 << OriginalText.size() << "\n";

    // Retrieve collected global declarations from our stored consumer pointer.
    std::vector<std::string> GlobalDecls;
    if (ConsumerPtr) {
      GlobalDecls = ConsumerPtr->getGlobalDecls();
      llvm::errs() << "[FrontendAction] Retrieved " << GlobalDecls.size()
                   << " global declarations.\n";
    } else {
      llvm::errs() << "[FrontendAction] ERROR: ConsumerPtr is null!\n";
      return;
    }

    std::string GlobalText;
    for (const auto &decl : GlobalDecls) {
      GlobalText += decl;
      llvm::errs() << "[FrontendAction] Global declaration: " << decl;
    }

    // Create a Replacement to insert the global declarations at the start of
    // the main file.
    Replacement GlobalInsert(SM, SM.getLocForStartOfFile(MainFID), 0,
                             GlobalText);
    if (llvm::Error Err = Reps.add(GlobalInsert))
      llvm::errs() << "[FrontendAction] Error adding global decl replacement: "
                   << llvm::toString(std::move(Err)) << "\n";
    else
      llvm::errs() << "[FrontendAction] Global decl replacement added.\n";

    // Log all recorded replacements.
    llvm::errs() << "[FrontendAction] Total replacements: " << Reps.size()
                 << "\n";
    for (const auto &R : Reps) {
      llvm::errs() << "[FrontendAction] Replacement: offset=" << R.getOffset()
                   << ", length=" << R.getLength() << " => '"
                   << R.getReplacementText() << "'\n";
    }

    // Apply all replacements.
    llvm::Expected<std::string> ChangedText =
        applyAllReplacements(OriginalText, Reps);
    if (!ChangedText) {
      llvm::errs() << "[FrontendAction] Error applying replacements: "
                   << llvm::toString(ChangedText.takeError()) << "\n";
      return;
    }
    llvm::errs() << "[FrontendAction] Replacements applied successfully. Final "
                    "text length: "
                 << ChangedText->size() << "\n";

    // Write output.
    if (OutputFilename.empty()) {
      llvm::outs() << *ChangedText;
      llvm::errs() << "[FrontendAction] Final output written to stdout.\n";
    } else {
      std::error_code EC;
      llvm::raw_fd_ostream outFile(OutputFilename, EC, llvm::sys::fs::OF_None);
      if (EC) {
        llvm::errs() << "[FrontendAction] Error opening output file: "
                     << EC.message() << "\n";
        return;
      }
      outFile << *ChangedText;
      llvm::errs() << "[FrontendAction] Final output written to file: "
                   << OutputFilename << "\n";
    }
  }

  std::unique_ptr<ASTConsumer> CreateASTConsumer(CompilerInstance &CI,
                                                 StringRef InFile) override {
    llvm::errs() << "[FrontendAction] Creating ASTConsumer for file: " << InFile
                 << "\n";
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
  llvm::errs() << "[main] Starting tool with " << argc << " arguments.\n";
  auto OptionsParserExpected =
      CommonOptionsParser::create(argc, argv, MyToolCategory);
  if (!OptionsParserExpected) {
    llvm::errs() << "[main] Error creating CommonOptionsParser: "
                 << llvm::toString(OptionsParserExpected.takeError()) << "\n";
    return 1;
  }
  CommonOptionsParser OptionsParser = std::move(OptionsParserExpected.get());
  llvm::errs() << "[main] Parsed options. Number of source paths: "
               << OptionsParser.getSourcePathList().size() << "\n";
  ClangTool Tool(OptionsParser.getCompilations(),
                 OptionsParser.getSourcePathList());
  llvm::errs() << "[main] Running frontend action...\n";
  int Result =
      Tool.run(newFrontendActionFactory<ReplacementFrontendAction>().get());
  llvm::errs() << "[main] Tool run completed with result: " << Result << "\n";
  return Result;
}
