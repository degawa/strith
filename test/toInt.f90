program test_toInt
    use, intrinsic :: iso_fortran_env
    use :: strith_parameter
    use :: strith_util_toInt
    implicit none

    integer(int8) :: i8
    integer(int16) :: i16
    integer(int32) :: i32
    integer(int64) :: i64

    !--- int8
    i8 = to_int8("+000000000000000000000000000000000000000")
    if (i8 == 0_int8) then
        print '(A)', "✅PASSED: to_int8(0)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(0). expected 0 but got ", i8
    end if

    i8 = to_int8("+000000000000000000000000000000000000001")
    if (i8 == 1_int8) then
        print '(A)', "✅PASSED: to_int8(1)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(1). expected 1 but got ", i8
    end if

    i8 = to_int8("+000000000000000000000000000000000000127")
    if (i8 == 127_int8) then
        print '(A)', "✅PASSED: to_int8(127)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(127). expected 127 but got ", i8
    end if

    i8 = to_int8("-000000000000000000000000000000000000000")
    if (i8 == 0_int8) then
        print '(A)', "✅PASSED: to_int8(-0)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(-0). expected 0 but got ", i8
    end if

    i8 = to_int8("-000000000000000000000000000000000000001")
    if (i8 == -1_int8) then
        print '(A)', "✅PASSED: to_int8(-1)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(-1). expected -1 but got ", i8
    end if

    i8 = to_int8("-000000000000000000000000000000000000128")
    if (i8 == int(B"10000000", kind=int8)) then
        print '(A)', "✅PASSED: to_int8(-128)"
    else
        print '(A,I0)', "🔥FAILED: to_int8(-128). expected -128 but got ", i8
    end if

    i8 = to_int8("+000000000000000000000000000000000000128")
    if (int(i8, int16) /= 128_int16) then
        print '(A)', "✅EXPECTED FAILURE: to_int8(128) does not return 128"
    else
        print '(A,I0)', "🔥UNEXPECTED PASS: to_int8(128). expected not 128 but got ", i8
    end if

    !--- int16
    i16 = to_int16("+000000000000000000000000000000000000000")
    if (i16 == 0_int16) then
        print '(A)', "✅PASSED: to_int16(0)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(0). expected 0 but got ", i16
    end if

    i16 = to_int16("+000000000000000000000000000000000000001")
    if (i16 == 1_int16) then
        print '(A)', "✅PASSED: to_int16(1)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(1). expected 1 but got ", i16
    end if

    i16 = to_int16("+000000000000000000000000000000000032767")
    if (i16 == 32767_int16) then
        print '(A)', "✅PASSED: to_int16(32767)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(32767). expected 32767 but got ", i16
    end if

    i16 = to_int16("-000000000000000000000000000000000000000")
    if (i16 == 0_int16) then
        print '(A)', "✅PASSED: to_int16(-0)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(-0). expected 0 but got ", i16
    end if

    i16 = to_int16("-000000000000000000000000000000000000001")
    if (i16 == -1_int16) then
        print '(A)', "✅PASSED: to_int16(-1)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(-1). expected -1 but got ", i16
    end if

    i16 = to_int16("-000000000000000000000000000000000032768")
    if (i16 == int(B"1000000000000000", kind=int16)) then
        print '(A)', "✅PASSED: to_int16(-32768)"
    else
        print '(A,I0)', "🔥FAILED: to_int16(-32768). expected -32768 but got ", i16
    end if

    i16 = to_int16("+000000000000000000000000000000000032768")
    if (int(i16, int32) /= 32768_int32) then
        print '(A)', "✅EXPECTED FAILURE: to_int16(32768) does not return 32768"
    else
        print '(A,I0)', "🔥UNEXPECTED PASS: to_int16(32768). expected not 32768 but got ", i16
    end if

    !--- int32
    i32 = to_int32("+000000000000000000000000000000000000000")
    if (i32 == 0_int32) then
        print '(A)', "✅PASSED: to_int32(0)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(0). expected 0 but got ", i32
    end if

    i32 = to_int32("+000000000000000000000000000000000000001")
    if (i32 == 1_int32) then
        print '(A)', "✅PASSED: to_int32(1)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(1). expected 1 but got ", i32
    end if

    i32 = to_int32("+000000000000000000000000000002147483647")
    if (i32 == 2147483647_int32) then
        print '(A)', "✅PASSED: to_int32(2147483647)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(2147483647). expected 2147483647 but got ", i32
    end if

    i32 = to_int32("-000000000000000000000000000000000000000")
    if (i32 == 0_int32) then
        print '(A)', "✅PASSED: to_int32(-0)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(-0). expected 0 but got ", i32
    end if

    i32 = to_int32("-000000000000000000000000000000000000001")
    if (i32 == -1_int32) then
        print '(A)', "✅PASSED: to_int32(-1)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(-1). expected -1 but got ", i32
    end if

    i32 = to_int32("-000000000000000000000000000002147483648")
    if (i32 == int(B"10000000000000000000000000000000", kind=int32)) then
        print '(A)', "✅PASSED: to_int32(-2147483648)"
    else
        print '(A,I0)', "🔥FAILED: to_int32(-2147483648). expected -2147483648 but got ", i32
    end if

    i32 = to_int32("+000000000000000000000000000002147483648")
    if (int(i32, int64) /= 2147483648_int64) then
        print '(A)', "✅EXPECTED FAILURE: to_int32(2147483648) does not return 2147483648"
    else
        print '(A,I0)', "🔥UNEXPECTED PASS: to_int32(2147483648). expected not 2147483648 but got ", i32
    end if

    !--- int64
    i64 = to_int64("+000000000000000000000000000000000000000")
    if (i64 == 0_int64) then
        print '(A)', "✅PASSED: to_int64(0)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(0). expected 0 but got ", i64
    end if

    i64 = to_int64("+000000000000000000000000000000000000001")
    if (i64 == 1_int64) then
        print '(A)', "✅PASSED: to_int64(1)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(1). expected 1 but got ", i64
    end if

    i64 = to_int64("+000000000000000000009223372036854775807")
    if (i64 == 9223372036854775807_int64) then
        print '(A)', "✅PASSED: to_int64(9223372036854775807)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(9223372036854775807). expected 9223372036854775807 but got ", i64
    end if

    i64 = to_int64("-000000000000000000000000000000000000000")
    if (i64 == 0_int64) then
        print '(A)', "✅PASSED: to_int64(-0)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(-0). expected 0 but got ", i64
    end if

    i64 = to_int64("-000000000000000000000000000000000000001")
    if (i64 == -1_int64) then
        print '(A)', "✅PASSED: to_int64(-1)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(-1). expected -1 but got ", i64
    end if

    i64 = to_int64("-000000000000000000009223372036854775808")
    if (i64 == int(B"1000000000000000000000000000000000000000000000000000000000000000", kind=int64)) then
        print '(A)', "✅PASSED: to_int64(-9223372036854775808)"
    else
        print '(A,I0)', "🔥FAILED: to_int64(-9223372036854775808). expected -9223372036854775808 but got ", i64
    end if

    i64 = to_int64("+000000000000000000009223372036854775808")
    if (i64 == 0_int64) then
        print '(A)', "✅EXPECTED FAILURE: to_int64(9223372036854775808) does not return 9223372036854775808"
    else
        print '(A,I0)', "🔥UNEXPECTED PASS: to_int64(9223372036854775808). expected not 9223372036854775808 but got ", i64
    end if
end program test_toInt
