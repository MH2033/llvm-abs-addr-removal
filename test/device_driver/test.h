#include <stdint.h>

typedef struct {
    uint32_t status_register;
    uint32_t command_register;
} dev_driver_regs;


#define IO_DEV_DRIVER_MEMORY ((void*)0xF0003000u)
#define IO_DEV_DRIVER_SIZE 1024

#define IO_DEV_DRIVER_REGS (*(volatile dev_driver_regs*)0xF1003000u)