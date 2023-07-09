# Strith
Converts a variable representing a long integer into a string by performing arithmetic operations on numbers in strings.

## Motivations
There are no integers more significant than 64 bits in the Fortran standard. Creating user-defined types or libraries that handle longer integers, such as 128-bit and multiple-length integers, is possible. However, depending on their internal representation, printing them as an integer is challenging.

This library, Strith, aims to provide the functions to convert integers to strings regardless of their internal representation.

## Overview
The Strith provides a functions to handle 39-digit + sign integers written in strings (called `strint`). The primary purpose is to provide the functions to convert integers to `strint` and also provides some arithmetic and comparison operators required for the conversion.

The current version of the Strith supports four kinds of integer, i.e., integer(int8), integer(int16), integer(int32), and integer(int64), but any type can be acceptable with a conversion procedure.

### Examples
- Convert integer to string

```Fortran
use :: strith
print *, to_string(huge(0_int8)) ! +000000000000000000000000000000000000127
print *, to_string(huge(0_int8), remove_0_padding=.true.) ! 127

print *, to_string(int(B"11111111", kind=int8)) ! -000000000000000000000000000000000000001
print *, to_string(int(B"11111111", kind=int8), remove_0_padding=.true.) ! -1

print *, to_string(int(B"11111111", kind=int8), remove_0_padding=.true., as_unsigned=.true.) ! 255
```

- Convert User-defined type (64-bit integer consisting of two 32-bit integers) to string
```Fortran
program to_string_udt
    use, intrinsic :: iso_fortran_env
    use :: strith
    implicit none

    !>user-defined 64-bit integer
    type :: long
        integer(int32) :: upper !! upper 32-bit
        integer(int32) :: lower !! lower 32-bit
    end type

    type(long) :: l
    l%lower = int(Z"FFFFFFFF")
    l%upper = int(Z"FFFFFFFF")

    print *, to_string(l, udt_to_string, remove_0_padding=.true., as_unsigned=.true.) ! 18446744073709551615
    print *, to_string(l, udt_to_string, remove_0_padding=.true.) ! -1
contains
    !>Returns the converted string `str` from `var`.
    !>`as_unsigned` is automatically passed when this procedure is called back in `to_string`.
    subroutine udt_to_string(var, as_unsigned, str)
        implicit none
        class(*), intent(in) :: var
            !! a variable to be converted to a string
        logical, intent(in) :: as_unsigned
            !! a flag used to branch whether to convert as an unsigned integer
        character(len=digits), intent(inout) :: str
            !! the string converted from `var`

        integer(int32) :: i, bit

        str = zero
        select type (var); type is (long)
            ! convert integers represented by each bit to string and accumulate to str
            ! lower 32 bits
            do i = 0, bit_size(var%lower) - 1
                bit = ibits(var%lower, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i) + str
                end if
            end do
            ! upper 31 bits
            do i = 0, bit_size(var%upper) - 2
                bit = ibits(var%upper, pos=i, len=1)
                if (bit == 1) then
                    str = weights_of_digits(i + bit_size(var%lower)) + str
                end if
            end do
            ! most significant bit
            i = bit_size(var%upper) - 1
            bit = ibits(var%upper, pos=i, len=1)
            if (as_unsigned) then
                if (bit == 1) str = weights_of_digits(i + bit_size(var%lower)) + str
            else
                if (bit == 1) str = str - weights_of_digits(i + bit_size(var%lower))
            end if
        end select
    end subroutine udt_to_string
end program to_string_udt
```

## Getting started
### Requirements
Strith has been tested only on Windows 10 but may also work on Linux/MacOS. The compilers and versions listed below have been used to develop Strith.

- A Fortran 2008 compiler
    - gfortran 11.2 bundled with quickstart Fortran on Windows
    - Intel Fortran Classic 2021.5.0 Build 20211109_000000
    - NAG Fortran 7.1 Build 7117
- Fortran Package Manager (fpm) 0.7.0 alpha

#### Get the code
To get the code, execute the following commnad:
```console
git clone https://github.com/degawa/strith.git
cd strish
```

#### Build with fpm
To build the library using fpm, execute the following command:
```console
fpm build
```

#### Reference from your project
Add the following use statement to modules or procedures calling Strith.

```Fortran
use :: strith
```

#### Reference as a fpm project's dependency
To use Strith in your fpm project, add the following to the fpm.toml.

```toml
[dependencies]
strith = {git = "https://github.com/degawa/strith.git"}
```

## Todo
- [ ] To add docstrings.