/**************************************************************************/
/*  This file is part of BINSEC.                                          */
/*                                                                        */
/*  Copyright (C) 2026-2027                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  You can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#include "stdint.h"
#include "string.h"
#include "stdio.h"

#define HEADER_SIZE 8
#define BUF_SIZE 256

void get_msg(char *buf, char *input, uint64_t input_size)
{
    if(input_size <= BUF_SIZE + HEADER_SIZE)
    {
        //2) input_size < 8 => integer underflow
        uint64_t msg_size = input_size - HEADER_SIZE;
        //3) buffer overflow!!!
        memcpy(buf, input + HEADER_SIZE, msg_size);
    }
    else
        printf("Message too long!\n");
}

int main()
{
    //1) inputs: char *input, uint64_t input_size
    char input[1024];
    printf("Enter message:\n");
    scanf("%1024s", input);
    uint64_t input_size;
    printf("Enter message size:\n");
    scanf("%lu", &input_size);
    char buf[BUF_SIZE];
    get_msg(buf, input, input_size);
    return 0;
}
