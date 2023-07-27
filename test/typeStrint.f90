program test_typeStrint
    use, intrinsic :: iso_fortran_env
    use :: strith_type_strint
    use :: strith_parameter
    use :: strith_util_toString
    use :: strith_arithmetic_unary_negate
    implicit none

    type(strint_type) :: a, b, c
    character(len=digits) :: str_a, str_b
    character(:), allocatable :: str, expr

    call test_to_string()
    call test_new_strint()
    call test_add()
    call test_sub()
    call test_negate()
    call test_equal()
    call test_greater()
    call test_less()
    call test_abs()
    call test_assign()

contains
    subroutine test_to_string()
        str = a%to_string()
        if (str == "+000000000000000000000000000000000000000") then
            print '(A)', "✅PASSED: to_string"
        else
            print '(A)', "🔥FAILED: to_string. expected +000000000000000000000000000000000000000 but got "//str
        end if

        str = a%to_string(remove_0_padding=.true.)
        if (str == "0") then
            print '(A)', "✅PASSED: to_string with remove_0_padding=true"
        else
            print '(A)', "🔥FAILED: to_string with remove_0_padding=true. expected 0 but got "//str
        end if

        str = a%to_string(remove_0_padding=.false.)
        if (str == "+000000000000000000000000000000000000000") then
            print '(A)', "✅PASSED: to_string with remove_0_padding=false"
        else
            print '(A)', "🔥FAILED: to_string with remove_0_padding=false. "// &
                "expected +000000000000000000000000000000000000000 but got "//str
        end if
    end subroutine test_to_string

    subroutine test_new_strint()
        a = new_strint(int(B"01111110", kind=int8))
        str = a%to_string()
        if (str == "+000000000000000000000000000000000000126") then
            print '(A)', "✅PASSED: construct strint_type from int8"
        else
            print '(A)', "🔥FAILED: construct strint_type from int8. expected +000000000000000000000000000000000000126 "// &
                "but got "//str
        end if

        a = new_strint(int(B"0111111111111110", kind=int16))
        str = a%to_string()
        if (str == "+000000000000000000000000000000000032766") then
            print '(A)', "✅PASSED: construct strint_type from int16"
        else
            print '(A)', "🔥FAILED: construct strint_type from int16. expected +000000000000000000000000000000000032766 "// &
                "but got "//str
        end if

        a = new_strint(int(B"01111111111111111111111111111110", kind=int32))
        str = a%to_string()
        if (str == "+000000000000000000000000000002147483646") then
            print '(A)', "✅PASSED: construct strint_type from int32"
        else
            print '(A)', "🔥FAILED: construct strint_type from int32. expected +000000000000000000000000000002147483646 "// &
                "but got "//str
        end if

        a = new_strint(int(B"0111111111111111111111111111111111111111111111111111111111111110", kind=int64))
        str = a%to_string()
        if (str == "+000000000000000000009223372036854775806") then
            print '(A)', "✅PASSED: construct strint_type from int64"
        else
            print '(A)', "🔥FAILED: construct strint_type from int64. expected +000000000000000000009223372036854775806 "// &
                "but got "//str
        end if

        a = new_strint(weights_of_digits(62))
        str = a%to_string()
        if (str == weights_of_digits(62)) then
            print '(A)', "✅PASSED: construct strint_type from string"
        else
            print '(A)', "🔥FAILED: construct strint_type from string. expected "//weights_of_digits(62)//" but got "//str
        end if
    end subroutine test_new_strint

    subroutine test_add()
        ! add
        a = new_strint(weights_of_digits(127))
        b = new_strint(weights_of_digits(126))
        c = a + b
        str = c%to_string()
        expr = "170141183460469231731687303715884105728+85070591730234615865843651857942052864&
               &=255211775190703847597530955573826158592"
        if (str == "+255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint("-000000000000000000000000007420738134810")
        b = new_strint("+000000000000000000000000152125131763605")
        c = a + b
        str = c%to_string()
        expr = "-7420738134810+152125131763605=144704393628795"
        if (str == "+000000000000000000000000144704393628795") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint(to_string(7))
        b = new_strint(-(to_string(101)))
        c = a + b
        str = c%to_string()
        expr = "7+-101=-94"
        if (str == "-000000000000000000000000000000000000094") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(126))
        b = new_strint(-weights_of_digits(127))
        c = a + b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728+-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        !---
        ! add strint + str
        a = new_strint(weights_of_digits(127))
        str_b = weights_of_digits(126)
        c = a + str_b
        str = c%to_string()
        expr = "170141183460469231731687303715884105728+85070591730234615865843651857942052864&
               &=255211775190703847597530955573826158592"
        if (str == "+255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: strint+str "//expr
        else
            print '(A)', "🔥FAILED: strint+str. expected "//expr//", but got ", str
        end if

        a = new_strint("-000000000000000000000000007420738134810")
        str_b = "+000000000000000000000000152125131763605"
        c = a + str_b
        str = c%to_string()
        expr = "-7420738134810+152125131763605=144704393628795"
        if (str == "+000000000000000000000000144704393628795") then
            print '(A)', "✅PASSED: strint+str "//expr
        else
            print '(A)', "🔥FAILED: strint+str. expected "//expr//", but got ", str
        end if

        a = new_strint(to_string(7))
        str_b = -(to_string(101))
        c = a + str_b
        str = c%to_string()
        expr = "7+-101=-94"
        if (str == "-000000000000000000000000000000000000094") then
            print '(A)', "✅PASSED: strint+str "//expr
        else
            print '(A)', "🔥FAILED: strint+str. expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(126))
        str_b = -weights_of_digits(127)
        c = a + str_b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728+-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: strint+str "//expr
        else
            print '(A)', "🔥FAILED: strint+str. expected "//expr//", but got ", str
        end if

        ! add str + strint
        str_a = weights_of_digits(127)
        b = new_strint(weights_of_digits(126))
        c = str_a + b
        str = c%to_string()
        expr = "170141183460469231731687303715884105728+85070591730234615865843651857942052864&
               &=255211775190703847597530955573826158592"
        if (str == "+255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: str+strint "//expr
        else
            print '(A)', "🔥FAILED: str+strint. expected "//expr//", but got ", str
        end if

        str_a = "-000000000000000000000000007420738134810"
        b = new_strint("+000000000000000000000000152125131763605")
        c = str_a + b
        str = c%to_string()
        expr = "-7420738134810+152125131763605=144704393628795"
        if (str == "+000000000000000000000000144704393628795") then
            print '(A)', "✅PASSED: str+strint "//expr
        else
            print '(A)', "🔥FAILED: str+strint. expected "//expr//", but got ", str
        end if

        str_a = to_string(7)
        b = new_strint(-(to_string(101)))
        c = str_a + b
        str = c%to_string()
        expr = "7+-101=-94"
        if (str == "-000000000000000000000000000000000000094") then
            print '(A)', "✅PASSED: str+strint "//expr
        else
            print '(A)', "🔥FAILED: str+strint. expected "//expr//", but got ", str
        end if

        str_a = -weights_of_digits(126)
        b = new_strint(-weights_of_digits(127))
        c = str_a + b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728+-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: str+strint "//expr
        else
            print '(A)', "🔥FAILED: str+strint. expected "//expr//", but got ", str
        end if
    end subroutine test_add

    subroutine test_sub()
        ! sub
        a = new_strint(weights_of_digits(5))
        b = new_strint(weights_of_digits(3))
        c = a - b
        str = c%to_string()
        expr = "32+8=24"
        if (str == "+000000000000000000000000000000000000024") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint(weights_of_digits(10))
        b = new_strint(weights_of_digits(20))
        c = a - b
        str = c%to_string()
        expr = "1024-1048576=-1047552"
        if (str == "-000000000000000000000000000000001047552") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(127))
        b = new_strint(weights_of_digits(126))
        c = a - b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint("+000000000000000000000000007420738134810")
        b = new_strint("-000000000000000000000000152125131763605")
        c = a - b
        str = c%to_string()
        expr = "7420738134810--152125131763605=159545869898415"
        if (str == "+000000000000000000000000159545869898415") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(127))
        b = new_strint(-weights_of_digits(126))
        c = a - b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728--85070591730234615865843651857942052864&
               &=-85070591730234615865843651857942052864"
        if (str == "-085070591730234615865843651857942052864") then
            print '(A)', "✅PASSED: "//expr
        else
            print '(A)', "🔥FAILED: expected "//expr//", but got ", str
        end if

        !---
        ! sub strint-str
        a = new_strint(weights_of_digits(5))
        str_b = weights_of_digits(3)
        c = a - str_b
        str = c%to_string()
        expr = "32+8=24"
        if (str == "+000000000000000000000000000000000000024") then
            print '(A)', "✅PASSED: strint-str "//expr
        else
            print '(A)', "🔥FAILED: strint-str. expected "//expr//", but got ", str
        end if

        a = new_strint(weights_of_digits(10))
        str_b = weights_of_digits(20)
        c = a - str_b
        str = c%to_string()
        expr = "1024-1048576=-1047552"
        if (str == "-000000000000000000000000000000001047552") then
            print '(A)', "✅PASSED: strint-str "//expr
        else
            print '(A)', "🔥FAILED: strint-str. expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(127))
        str_b = weights_of_digits(126)
        c = a - str_b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: strint-str "//expr
        else
            print '(A)', "🔥FAILED: strint-str. expected "//expr//", but got ", str
        end if

        a = new_strint("+000000000000000000000000007420738134810")
        str_b = "-000000000000000000000000152125131763605"
        c = a - str_b
        str = c%to_string()
        expr = "7420738134810--152125131763605=159545869898415"
        if (str == "+000000000000000000000000159545869898415") then
            print '(A)', "✅PASSED: strint-str "//expr
        else
            print '(A)', "🔥FAILED: strint-str. expected "//expr//", but got ", str
        end if

        a = new_strint(-weights_of_digits(127))
        str_b = -weights_of_digits(126)
        c = a - str_b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728--85070591730234615865843651857942052864&
               &=-85070591730234615865843651857942052864"
        if (str == "-085070591730234615865843651857942052864") then
            print '(A)', "✅PASSED: strint-str "//expr
        else
            print '(A)', "🔥FAILED: strint-str. expected "//expr//", but got ", str
        end if

        ! sub str-strint
        str_a = weights_of_digits(5)
        b = new_strint(weights_of_digits(3))
        c = str_a - b
        str = c%to_string()
        expr = "32+8=24"
        if (str == "+000000000000000000000000000000000000024") then
            print '(A)', "✅PASSED: str-strint "//expr
        else
            print '(A)', "🔥FAILED: str-strint. expected "//expr//", but got ", str
        end if

        str_a = weights_of_digits(10)
        b = new_strint(weights_of_digits(20))
        c = str_a - b
        str = c%to_string()
        expr = "1024-1048576=-1047552"
        if (str == "-000000000000000000000000000000001047552") then
            print '(A)', "✅PASSED: str-strint "//expr
        else
            print '(A)', "🔥FAILED: str-strint. expected "//expr//", but got ", str
        end if

        str_a = -weights_of_digits(127)
        b = new_strint(weights_of_digits(126))
        c = str_a - b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728-85070591730234615865843651857942052864&
               &=-255211775190703847597530955573826158592"
        if (str == "-255211775190703847597530955573826158592") then
            print '(A)', "✅PASSED: str-strint "//expr
        else
            print '(A)', "🔥FAILED: str-strint. expected "//expr//", but got ", str
        end if

        str_a = "+000000000000000000000000007420738134810"
        b = new_strint("-000000000000000000000000152125131763605")
        c = str_a - b
        str = c%to_string()
        expr = "7420738134810--152125131763605=159545869898415"
        if (str == "+000000000000000000000000159545869898415") then
            print '(A)', "✅PASSED: str-strint "//expr
        else
            print '(A)', "🔥FAILED: str-strint. expected "//expr//", but got ", str
        end if

        str_a = -weights_of_digits(127)
        b = new_strint(-weights_of_digits(126))
        c = str_a - b
        str = c%to_string()
        expr = "-170141183460469231731687303715884105728--85070591730234615865843651857942052864&
               &=-85070591730234615865843651857942052864"
        if (str == "-085070591730234615865843651857942052864") then
            print '(A)', "✅PASSED: str-strint "//expr
        else
            print '(A)', "🔥FAILED: str-strint. expected "//expr//", but got ", str
        end if
    end subroutine test_sub

    subroutine test_negate()
        ! negate
        a = new_strint(weights_of_digits(1))
        c = -a
        str = c%to_string()
        if (str == "-000000000000000000000000000000000000002") then
            print '(A)', "✅PASSED: negate of positive value"
        else
            print '(A)', "🔥FAILED: negate of positive value. expected -000000000000000000000000000000000000002 but got "//str
        end if

        a = new_strint(-weights_of_digits(1))
        c = -a
        str = c%to_string()
        if (str == "+000000000000000000000000000000000000002") then
            print '(A)', "✅PASSED: negate of negative value"
        else
            print '(A)', "🔥FAILED: negate of negative value. expected +000000000000000000000000000000000000002 but got"//str
        end if
    end subroutine test_negate

    subroutine test_equal()
        ! equal
        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if (a == b) then
            print '(A)', "✅PASSED: 8==8 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8==8 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(4))
        if ((a == b) .eqv. .false.) then
            print '(A)', "✅PASSED: 8==16 is .false."
        else
            print '(A)', "🔥FAILED: expect that 8==16 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(4))
        b = new_strint(-weights_of_digits(4))
        if ((a == b) .eqv. .false.) then
            print '(A)', "✅PASSED: 16==-16 is .false."
        else
            print '(A)', "🔥FAILED: expect that 16==-16 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(4))
        b = new_strint(-weights_of_digits(4))
        if (a /= b) then
            print '(A)', "✅PASSED: 16/=-16 is .true."
        else
            print '(A)', "🔥FAILED: expect that 16/=-16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if ((a /= b) .eqv. .false.) then
            print '(A)', "✅PASSED: 8/=8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 8/=8 is .true., but got .false."
        end if

        !---
        ! equal strint==str
        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if (a == str_b) then
            print '(A)', "✅PASSED: strint==str 8==8 is .true."
        else
            print '(A)', "🔥FAILED: strint==str. expect that 8==8 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(4)
        if ((a == str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint==str 8==16 is .false."
        else
            print '(A)', "🔥FAILED: strint==str. expect that 8==16 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(4))
        str_b = -weights_of_digits(4)
        if ((a == str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint==str 16==-16 is .false."
        else
            print '(A)', "🔥FAILED: strint==str. expect that 16==-16 is .false., but got .true."
        end if

        ! equal str==strint
        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if (str_a == b) then
            print '(A)', "✅PASSED: str==strint 8==8 is .true."
        else
            print '(A)', "🔥FAILED: str==strint. expect that 8==8 is .true., but got .false."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(4))
        if ((str_a == b) .eqv. .false.) then
            print '(A)', "✅PASSED: str==strint 8==16 is .false."
        else
            print '(A)', "🔥FAILED: str==strint. expect that 8==16 is .false., but got .true."
        end if

        str_a = weights_of_digits(4)
        b = new_strint(-weights_of_digits(4))
        if ((str_a == b) .eqv. .false.) then
            print '(A)', "✅PASSED: str==strint 16==-16 is .false."
        else
            print '(A)', "🔥FAILED: str==strint. expect that 16==-16 is .false., but got .true."
        end if

        !---
        ! not equal strint/=str
        a = new_strint(weights_of_digits(4))
        str_b = -weights_of_digits(4)
        if (a /= str_b) then
            print '(A)', "✅PASSED: strint/=str 16/=-16 is .true."
        else
            print '(A)', "🔥FAILED: strint/=str. expect that 16/=-16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if ((a /= str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint/=str 8/=8 is .false."
        else
            print '(A)', "🔥FAILED: strint/=str. xpect that 8/=8 is .true., but got .false."
        end if

        ! not equal str/=strint
        str_a = weights_of_digits(4)
        b = new_strint(-weights_of_digits(4))
        if (str_a /= b) then
            print '(A)', "✅PASSED: str/=strint 16/=-16 is .true."
        else
            print '(A)', "🔥FAILED: str/=strint. expect that 16/=-16 is .true., but got .false."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if ((str_a /= b) .eqv. .false.) then
            print '(A)', "✅PASSED: str/=strint 8/=8 is .false."
        else
            print '(A)', "🔥FAILED: str/=strint. xpect that 8/=8 is .true., but got .false."
        end if
    end subroutine test_equal

    subroutine test_greater()
        ! greater
        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(2))
        if (a > b) then
            print '(A)', "✅PASSED: 8>4 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8>4 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(2))
        b = new_strint(weights_of_digits(3))
        if ((a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: 4>8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 4>8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if ((a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: 8>8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 8>8 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(2))
        b = new_strint(-weights_of_digits(3))
        if ((a > b) .eqv. .true.) then
            print '(A)', "✅PASSED: -4>-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -4>-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(2))
        if ((a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8>-4 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8>-4 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(3))
        if ((a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8>-8 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8>-8 is .false., but got .true."
        end if

        !---
        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(2))
        if (a >= b) then
            print '(A)', "✅PASSED: 8>=4 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8>=4 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(2))
        b = new_strint(weights_of_digits(3))
        if ((a >= b) .eqv. .false.) then
            print '(A)', "✅PASSED: 4>=8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 4>=8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if ((a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: 8>=8 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8>=8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(2))
        b = new_strint(-weights_of_digits(3))
        if ((a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: -4>=-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -4>=-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(2))
        if ((a >= b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8>=-4 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8>=-4 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(3))
        if ((a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: -8>=-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -8>=-8 is .true., but got .false."
        end if

        !---
        ! greater strint>str
        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(2)
        if (a > str_b) then
            print '(A)', "✅PASSED: strint>str 8>4 is .true."
        else
            print '(A)', "🔥FAILED: strint>str. expect that 8>4 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(2))
        str_b = weights_of_digits(3)
        if ((a > str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>str 4>8 is .false."
        else
            print '(A)', "🔥FAILED: strint>str. expect that 4>8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if ((a > str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>str 8>8 is .false."
        else
            print '(A)', "🔥FAILED: strint>str. expect that 8>8 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(2))
        str_b = -weights_of_digits(3)
        if ((a > str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint>str -4>-8 is .true."
        else
            print '(A)', "🔥FAILED: strint>str. expect that -4>-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(2)
        if ((a > str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>str -8>-4 is .false."
        else
            print '(A)', "🔥FAILED: strint>str. expect that -8>-4 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(3)
        if ((a > str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>str -8>-8 is .false."
        else
            print '(A)', "🔥FAILED: strint>str. expect that -8>-8 is .false., but got .true."
        end if

        ! greater str>strint
        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(2))
        if (str_a > b) then
            print '(A)', "✅PASSED: str>strint 8>4 is .true."
        else
            print '(A)', "🔥FAILED: str>strint. expect that 8>4 is .true., but got .false."
        end if

        str_a = weights_of_digits(2)
        b = new_strint(weights_of_digits(3))
        if ((str_a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>strint 4>8 is .false."
        else
            print '(A)', "🔥FAILED: str>strint. expect that 4>8 is .false., but got .true."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if ((str_a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>strint 8>8 is .false."
        else
            print '(A)', "🔥FAILED: str>strint. expect that 8>8 is .false., but got .true."
        end if

        str_a = -weights_of_digits(2)
        b = -new_strint(weights_of_digits(3))
        if ((str_a > b) .eqv. .true.) then
            print '(A)', "✅PASSED: str>strint -4>-8 is .true."
        else
            print '(A)', "🔥FAILED: str>strint. expect that -4>-8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(2))
        if ((str_a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>strint -8>-4 is .false."
        else
            print '(A)', "🔥FAILED: str>strint. expect that -8>-4 is .false., but got .true."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(3))
        if ((str_a > b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>strint -8>-8 is .false."
        else
            print '(A)', "🔥FAILED: str>strint. expect that -8>-8 is .false., but got .true."
        end if

        !---
        ! greater than or equal to strint>=str
        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(2)
        if (a >= str_b) then
            print '(A)', "✅PASSED: strint>=str 8>=4 is .true."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that 8>=4 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(2))
        str_b = weights_of_digits(3)
        if ((a >= str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>=str 4>=8 is .false."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that 4>=8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if ((a >= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint>=str 8>=8 is .true."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that 8>=8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(2))
        str_b = -weights_of_digits(3)
        if ((a >= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint>=str -4>=-8 is .true."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that -4>=-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(2)
        if ((a >= str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint>=str -8>=-4 is .false."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that -8>=-4 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(3)
        if ((a >= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint>=str -8>=-8 is .true."
        else
            print '(A)', "🔥FAILED: strint>=str. expect that -8>=-8 is .true., but got .false."
        end if

        ! greater than or equal to str>=strint
        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(2))
        if (str_a >= b) then
            print '(A)', "✅PASSED: str>=strint 8>=4 is .true."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that 8>=4 is .true., but got .false."
        end if

        str_a = weights_of_digits(2)
        b = new_strint(weights_of_digits(3))
        if ((str_a >= b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>=strint 4>=8 is .false."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that 4>=8 is .false., but got .true."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if ((str_a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str>=strint 8>=8 is .true."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that 8>=8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(2)
        b = new_strint(-weights_of_digits(3))
        if ((str_a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str>=strint -4>=-8 is .true."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that -4>=-8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(2))
        if ((str_a >= b) .eqv. .false.) then
            print '(A)', "✅PASSED: str>=strint -8>=-4 is .false."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that -8>=-4 is .false., but got .true."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(3))
        if ((str_a >= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str>=strint -8>=-8 is .true."
        else
            print '(A)', "🔥FAILED: str>=strint. expect that -8>=-8 is .true., but got .false."
        end if
    end subroutine test_greater

    subroutine test_less()
        ! less
        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(4))
        if ((a < b)) then
            print '(A)', "✅PASSED: 8<16 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8<16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(4))
        b = new_strint(weights_of_digits(3))
        if ((a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: 16<8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 16<8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if ((a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: 8<8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 8<8 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(4))
        b = new_strint(-weights_of_digits(3))
        if ((a < b) .eqv. .true.) then
            print '(A)', "✅PASSED: -16<-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -16<-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(4))
        if ((a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8<-16 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8<-16 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(3))
        if ((a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8<-8 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8<-8 is .false., but got .true."
        end if

        !---
        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(4))
        if ((a <= b)) then
            print '(A)', "✅PASSED: 8<=16 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8<=16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(4))
        b = new_strint(weights_of_digits(3))
        if ((a <= b) .eqv. .false.) then
            print '(A)', "✅PASSED: 16<=8 is .false."
        else
            print '(A)', "🔥FAILED: expect that 16<=8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        b = new_strint(weights_of_digits(3))
        if ((a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: 8<=8 is .true."
        else
            print '(A)', "🔥FAILED: expect that 8<=8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(4))
        b = new_strint(-weights_of_digits(3))
        if ((a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: -16<=-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -16<=-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(4))
        if ((a <= b) .eqv. .false.) then
            print '(A)', "✅PASSED: -8<=-16 is .false."
        else
            print '(A)', "🔥FAILED: expect that -8<=-16 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        b = new_strint(-weights_of_digits(3))
        if ((a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: -8<=-8 is .true."
        else
            print '(A)', "🔥FAILED: expect that -8<=-8 is .true., but got .false."
        end if

        !---
        ! less strint<str
        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(4)
        if ((a < str_b)) then
            print '(A)', "✅PASSED: strint<str 8<16 is .true."
        else
            print '(A)', "🔥FAILED: strint<str. expect that 8<16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(4))
        str_b = weights_of_digits(3)
        if ((a < str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<str 16<8 is .false."
        else
            print '(A)', "🔥FAILED: strint<str. expect that 16<8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if ((a < str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<str 8<8 is .false."
        else
            print '(A)', "🔥FAILED: strint<str. expect that 8<8 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(4))
        str_b = -weights_of_digits(3)
        if ((a < str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint<str -16<-8 is .true."
        else
            print '(A)', "🔥FAILED: strint<str. expect that -16<-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(4)
        if ((a < str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<str -8<-16 is .false."
        else
            print '(A)', "🔥FAILED: strint<str. expect that -8<-16 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(3)
        if ((a < str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<str -8<-8 is .false."
        else
            print '(A)', "🔥FAILED: strint<str. expect that -8<-8 is .false., but got .true."
        end if

        ! less str<strint
        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(4))
        if ((str_a < b)) then
            print '(A)', "✅PASSED: str<strint 8<16 is .true."
        else
            print '(A)', "🔥FAILED: str<strint. expect that 8<16 is .true., but got .false."
        end if

        str_a = weights_of_digits(4)
        b = new_strint(weights_of_digits(3))
        if ((str_a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<strint 16<8 is .false."
        else
            print '(A)', "🔥FAILED: str<strint. expect that 16<8 is .false., but got .true."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if ((str_a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<strint 8<8 is .false."
        else
            print '(A)', "🔥FAILED: str<strint. expect that 8<8 is .false., but got .true."
        end if

        str_a = -weights_of_digits(4)
        b = new_strint(-weights_of_digits(3))
        if ((str_a < b) .eqv. .true.) then
            print '(A)', "✅PASSED: str<strint -16<-8 is .true."
        else
            print '(A)', "🔥FAILED: str<strint. expect that -16<-8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(4))
        if ((str_a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<strint -8<-16 is .false."
        else
            print '(A)', "🔥FAILED: str<strint. expect that -8<-16 is .false., but got .true."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(3))
        if ((str_a < b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<strint -8<-8 is .false."
        else
            print '(A)', "🔥FAILED: str<strint. expect that -8<-8 is .false., but got .true."
        end if

        !---
        ! less than or equal to strint<=str
        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(4)
        if ((a <= str_b)) then
            print '(A)', "✅PASSED: strint<=str 8<=16 is .true."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that 8<=16 is .true., but got .false."
        end if

        a = new_strint(weights_of_digits(4))
        str_b = weights_of_digits(3)
        if ((a <= str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<=str 16<=8 is .false."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that 16<=8 is .false., but got .true."
        end if

        a = new_strint(weights_of_digits(3))
        str_b = weights_of_digits(3)
        if ((a <= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint<=str 8<=8 is .true."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that 8<=8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(4))
        str_b = -weights_of_digits(3)
        if ((a <= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint<=str -16<=-8 is .true."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that -16<=-8 is .true., but got .false."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(4)
        if ((a <= str_b) .eqv. .false.) then
            print '(A)', "✅PASSED: strint<=str -8<=-16 is .false."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that -8<=-16 is .false., but got .true."
        end if

        a = new_strint(-weights_of_digits(3))
        str_b = -weights_of_digits(3)
        if ((a <= str_b) .eqv. .true.) then
            print '(A)', "✅PASSED: strint<=str -8<=-8 is .true."
        else
            print '(A)', "🔥FAILED: strint<=str. expect that -8<=-8 is .true., but got .false."
        end if

        ! less than or equal to str<=strint
        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(4))
        if ((str_a <= b)) then
            print '(A)', "✅PASSED: str<=strint 8<=16 is .true."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that 8<=16 is .true., but got .false."
        end if

        str_a = weights_of_digits(4)
        b = new_strint(weights_of_digits(3))
        if ((str_a <= b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<=strint 16<=8 is .false."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that 16<=8 is .false., but got .true."
        end if

        str_a = weights_of_digits(3)
        b = new_strint(weights_of_digits(3))
        if ((str_a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str<=strint 8<=8 is .true."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that 8<=8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(4)
        b = new_strint(-weights_of_digits(3))
        if ((str_a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str<=strint -16<=-8 is .true."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that -16<=-8 is .true., but got .false."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(4))
        if ((str_a <= b) .eqv. .false.) then
            print '(A)', "✅PASSED: str<=strint -8<=-16 is .false."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that -8<=-16 is .false., but got .true."
        end if

        str_a = -weights_of_digits(3)
        b = new_strint(-weights_of_digits(3))
        if ((str_a <= b) .eqv. .true.) then
            print '(A)', "✅PASSED: str<=strint -8<=-8 is .true."
        else
            print '(A)', "🔥FAILED: str<=strint. expect that -8<=-8 is .true., but got .false."
        end if
    end subroutine test_less

    subroutine test_abs()
        ! abs
        a = new_strint(-weights_of_digits(8))
        c = abs(a)
        str = c%to_string()
        if (str == "+000000000000000000000000000000000000256") then
            print '(A)', "✅PASSED: abs(negative value) returns the positive value"
        else
            print '(A)', "🔥FAILED: abs(negative value) returns the positive value. "// &
                "expected +000000000000000000000000000000000000256 but got "//str
        end if

        a = new_strint(weights_of_digits(8))
        c = abs(a)
        str = c%to_string()
        if (str == "+000000000000000000000000000000000000256") then
            print '(A)', "✅PASSED: abs(positive value) returns the positive value"
        else
            print '(A)', "🔥FAILED: abs(positive value) returns the positive value. "// &
                "expected +000000000000000000000000000000000000256 but got "//str
        end if
    end subroutine test_abs

    subroutine test_assign()
        ! assign
        a = weights_of_digits(127)
        if (a%to_string() == "+170141183460469231731687303715884105728") then
            print '(A)', "✅PASSED: assignment string to strint"
        else
            print '(A)', "🔥FAILED: assignment string to strint. "// &
                "expected +170141183460469231731687303715884105728, but got ", a%to_string()
        end if
    end subroutine test_assign
end program test_typeStrint
