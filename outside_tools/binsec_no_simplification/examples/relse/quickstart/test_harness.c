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

#include <stdlib.h>

int memcmp(const void *s1, const void *s2, size_t n);

#define SIZE (1 << 4) /* 16B */
char s1[SIZE], s2[SIZE];
size_t n = SIZE;

int main(int argc, char *argv[])
{
  int res = memcmp(s1, s2, n);

  /* ensure the result is in [-1..1] */
  res |= res >> 1;
  res |= res >> 2;
  res |= res >> 4;
  res |= res >> 8;
  res |= res >> 16;
  res = (res & 1) | (res >> 31);

  exit(res);
}
