program test_toString
    use, intrinsic :: iso_fortran_env
    use :: strith_util_toString
    implicit none

    character(:), allocatable :: str

    !--- naive
    str = to_string(huge(0_int8))
    if (str == "+000000000000000000000000000000000000127") then
        print '(A)', "✅PASSED: to_string for int8"
    else
        print '(A)', "🔥FAILED: to_string for int8. expected +000000000000000000000000000000000000127 but got"//str
    end if

    str = to_string(int(B"11111111", kind=int8))
    if (str == "-000000000000000000000000000000000000001") then
        print '(A)', "✅PASSED: to_string for negative int8"
    else
        print '(A)', "🔥FAILED: to_string for negative int8. expected -000000000000000000000000000000000000001 but got"//str
    end if

    str = to_string(huge(0_int16))
    if (str == "+000000000000000000000000000000000032767") then
        print '(A)', "✅PASSED: to_string for int16"
    else
        print '(A)', "🔥FAILED: to_string for int16. expected +000000000000000000000000000000000032767 but got"//str
    end if

    str = to_string(int(B"1111111111111111", kind=int16))
    if (str == "-000000000000000000000000000000000000001") then
        print '(A)', "✅PASSED: to_string for negative int16"
    else
        print '(A)', "🔥FAILED: to_string for negative int16. expected -000000000000000000000000000000000000001 but got"//str
    end if

    str = to_string(huge(0_int32))
    if (str == "+000000000000000000000000000002147483647") then
        print '(A)', "✅PASSED: to_string for int32"
    else
        print '(A)', "🔥FAILED: to_string for int32. expected +000000000000000000000000000002147483647 but got"//str
    end if

    str = to_string(int(B"11111111111111111111111111111111", kind=int32))
    if (str == "-000000000000000000000000000000000000001") then
        print '(A)', "✅PASSED: to_string for negative int32."
    else
        print '(A)', "🔥FAILED: to_string for negative int32. expected -000000000000000000000000000000000000001 but got"//str
    end if

    str = to_string(huge(0_int64))
    if (str == "+000000000000000000009223372036854775807") then
        print '(A)', "✅PASSED: to_string for int64"
    else
        print '(A)', "🔥FAILED: to_string for int64. expected +000000000000000000009223372036854775807 but got"//str
    end if

    str = to_string(int(B"1111111111111111111111111111111111111111111111111111111111111111", kind=int64))
    if (str == "-000000000000000000000000000000000000001") then
        print '(A)', "✅PASSED: to_string for negative int64."
    else
        print '(A)', "🔥FAILED: to_string for negative int64. expected -000000000000000000000000000000000000001 but got"//str
    end if

    !--- remove 0 padding
    str = to_string(huge(0_int8), remove_0_padding=.true.)
    if (str == "127") then
        print '(A)', "✅PASSED: to_string for int8 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for int8 with remove 0 padding. expected 127 but got"//str
    end if

    str = to_string(int(B"11111111", kind=int8), remove_0_padding=.true.)
    if (str == "-1") then
        print '(A)', "✅PASSED: to_string for negative int8 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for negative int8 with remove 0 padding. expected -1 but got"//str
    end if

    str = to_string(huge(0_int16), remove_0_padding=.true.)
    if (str == "32767") then
        print '(A)', "✅PASSED: to_string for int16 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for int16 with remove 0 padding. expected 32767 but got"//str
    end if

    str = to_string(int(B"1111111111111111", kind=int16), remove_0_padding=.true.)
    if (str == "-1") then
        print '(A)', "✅PASSED: to_string for negative int16 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for negative int16 with remove 0 padding. expected -1 but got"//str
    end if

    str = to_string(huge(0_int32), remove_0_padding=.true.)
    if (str == "2147483647") then
        print '(A)', "✅PASSED: to_string for int32 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for int32 with remove 0 padding. expected 2147483647 but got"//str
    end if

    str = to_string(int(B"11111111111111111111111111111111", kind=int32), remove_0_padding=.true.)
    if (str == "-1") then
        print '(A)', "✅PASSED: to_string for negative int32 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for negative int32 with remove 0 padding. expected -1 but got"//str
    end if

    str = to_string(huge(0_int64), remove_0_padding=.true.)
    if (str == "9223372036854775807") then
        print '(A)', "✅PASSED: to_string for int64 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for int64 with remove 0 padding. expected 9223372036854775807 but got"//str
    end if

    str = to_string(int(B"1111111111111111111111111111111111111111111111111111111111111111", kind=int64), remove_0_padding=.true.)
    if (str == "-1") then
        print '(A)', "✅PASSED: to_string for negative int64 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for negative int64 with remove 0 padding. expected -1 but got"//str
    end if

    !--- as unsigned
    str = to_string(int(B"11111111", kind=int8), as_unsigned=.true.)
    if (str == "+000000000000000000000000000000000000255") then
        print '(A)', "✅PASSED: to_string for unsigned int8"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int8. "// &
            "expected +000000000000000000000000000000000000255 but got"//str
    end if

    str = to_string(int(B"1111111111111111", kind=int16), as_unsigned=.true.)
    if (str == "+000000000000000000000000000000000065535") then
        print '(A)', "✅PASSED: to_string for unsigned int16"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int16. expected +000000000000000000000000000000000065535 but got"//str
    end if

    str = to_string(int(B"11111111111111111111111111111111", kind=int32), as_unsigned=.true.)
    if (str == "+000000000000000000000000000004294967295") then
        print '(A)', "✅PASSED: to_string for unsigned int32"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int32. expected +000000000000000000000000000004294967295 but got"//str
    end if

    str = to_string(int(B"1111111111111111111111111111111111111111111111111111111111111111", kind=int64), as_unsigned=.true.)
    if (str == "+000000000000000000018446744073709551615") then
        print '(A)', "✅PASSED: to_string for unsigned int64"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int64. expected +000000000000000000018446744073709551615 but got"//str
    end if

    !--- remove 0 padding and as unsigned
    str = to_string(int(B"11111111", kind=int8), remove_0_padding=.true., as_unsigned=.true.)
    if (str == "255") then
        print '(A)', "✅PASSED: to_string for unsigned int8 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int8 with remove 0 padding. "// &
            "expected 255 but got"//str
    end if

    str = to_string(int(B"1111111111111111", kind=int16), remove_0_padding=.true., as_unsigned=.true.)
    if (str == "65535") then
        print '(A)', "✅PASSED: to_string for unsigned int16 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int16 with remove 0 padding. expected 65535 but got"//str
    end if

    str = to_string(int(B"11111111111111111111111111111111", kind=int32), remove_0_padding=.true., as_unsigned=.true.)
    if (str == "4294967295") then
        print '(A)', "✅PASSED: to_string for unsigned int32 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int32 with remove 0 padding. expected 4294967295 but got"//str
    end if

    str = to_string(int(B"1111111111111111111111111111111111111111111111111111111111111111", kind=int64), &
                    remove_0_padding=.true., as_unsigned=.true.)
    if (str == "18446744073709551615") then
        print '(A)', "✅PASSED: to_string for unsigned int64 with remove 0 padding"
    else
        print '(A)', "🔥FAILED: to_string for unsigned int64 with remove 0 padding. expected 18446744073709551615 but got"//str
    end if
end program test_toString
