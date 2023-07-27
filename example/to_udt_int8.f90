program ex_strint_to_int8
    use, intrinsic :: iso_fortran_env
    use :: strith
    implicit none

    integer(int8) :: i8

    call to_udt_int8(to_string(-127), i8)
    print '(B8.8)', i8
contains
    subroutine to_udt_int8(str, var)
        implicit none
        character(len=digits), intent(in) :: str
        class(*), intent(inout) :: var

        type(strint_type) :: val

        select type (var)
        type is (integer(int8))
            var = 0_int8
            val = str

            if (val == zero) return

            if (val > zero) then
                call strint_to_int8(val, var)
                return
            end if

            if (val < zero) then
                val = abs(str + one)
                call strint_to_int8(val, var)
                var = int(ibset(var, 7), kind=int8)
            end if
        end select
    end subroutine to_udt_int8

    subroutine strint_to_int8(val, var)
        type(strint_type), intent(inout) :: val
        integer(int8), intent(inout) :: var

        integer(int32) :: digit
        do digit = 7, 0, -1
            if (val >= weights_of_digits(digit)) then !&
                var = int(ibset(var, digit), kind=int8)
                val = val - weights_of_digits(digit)
            else
                var = int(ibclr(var, digit), kind=int8)
            end if
        end do
    end subroutine strint_to_int8
end program ex_strint_to_int8
