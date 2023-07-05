program test_toString
    use, intrinsic :: iso_fortran_env
    use :: strith_toString
    implicit none

    character(:), allocatable :: str

    str = to_string(huge(0_int8))
    if (str == "+000000000000000000000000000000000000127") then
        print *, "PASSED: to_string for int8"
    else
        print *, "FAILED: to_string for int8. expected +000000000000000000000000000000000000127 but got"//str
    end if

    str = to_string(int(B"11111111", kind=int8))
    if (str == "+000000000000000000000000000000000000255") then
        print *, "EXPECTED FAILURE: to_string for int8 currently cannot convert negative values."
    else if (str == "-000000000000000000000000000000000000128") then
        print *, "UNEXPECTED PASS: to_string for int8"
    else
        print *, "FAILED: to_string for int8. expected +000000000000000000000000000000000000255 but got"//str
    end if

    str = to_string(huge(0_int16))
    if (str == "+000000000000000000000000000000000032767") then
        print *, "PASSED: to_string for int16"
    else
        print *, "FAILED: to_string for int16. expected +000000000000000000000000000000000032767 but got"//str
    end if

    str = to_string(huge(0_int32))
    if (str == "+000000000000000000000000000002147483647") then
        print *, "PASSED: to_string for int32"
    else
        print *, "FAILED: to_string for int32. expected +000000000000000000000000000002147483647 but got"//str
    end if

    str = to_string(huge(0_int64))
    if (str == "+000000000000000000009223372036854775807") then
        print *, "PASSED: to_string for int64"
    else
        print *, "FAILED: to_string for int64. expected +000000000000000000009223372036854775807 but got"//str
    end if

    str = to_string(huge(0_int8), remove_0_padding=.true.)
    if (str == "127") then
        print *, "PASSED: to_string for int8 with remove 0 padding"
    else
        print *, "FAILED: to_string for int8 with remove 0 padding. expected 127 but got"//str
    end if

    str = to_string(int(B"11111111", kind=int8), remove_0_padding=.true.)
    if (str == "255") then
        print *, "EXPECTED FAILURE: to_string for int8 currently cannot convert negative values."
    else if (str == "-128") then
        print *, "UNEXPECTED PASS: to_string for int8"
    else
        print *, "FAILED: to_string for int8. expected 255 but got"//str
    end if

    str = to_string(huge(0_int16), remove_0_padding=.true.)
    if (str == "32767") then
        print *, "PASSED: to_string for int16 with remove 0 padding"
    else
        print *, "FAILED: to_string for int16 with remove 0 padding. expected 32767 but got"//str
    end if

    str = to_string(huge(0_int32), remove_0_padding=.true.)
    if (str == "2147483647") then
        print *, "PASSED: to_string for int32 with remove 0 padding"
    else
        print *, "FAILED: to_string for int32 with remove 0 padding. expected 2147483647 but got"//str
    end if

    str = to_string(huge(0_int64), remove_0_padding=.true.)
    if (str == "9223372036854775807") then
        print *, "PASSED: to_string for int64 with remove 0 padding"
    else
        print *, "FAILED: to_string for int64 with remove 0 padding. expected 9223372036854775807 but got"//str
    end if
end program test_toString
