
#include <stdint.h>
#include "test.h"



int init_driver() {

    IO_DEV_DRIVER_REGS.command_register = 0x01;

    uint32_t *driver_memory = (uint32_t *) IO_DEV_DRIVER_MEMORY;

    if((IO_DEV_DRIVER_REGS.status_register & 0x10) == 0) {
        driver_memory[0] = 15;
        return 1;

    } else {
        for(int i = 0; i < IO_DEV_DRIVER_SIZE / sizeof(uint32_t); i++) {
            driver_memory[i] = 0;
        }
        return 0;
    }
}

int main()
{
    init_driver();
}