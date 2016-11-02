    !-------------------------------------------------------------------
    ! Hand-written overloaded definition for 'huge'
    !
    ! This function behaves differently than others for reduced
    ! precision. If we simply round the result of the HUGE intrinsic we
    ! will always yield infinity, since HUGE will return a value with
    ! a full significand.
    !
    ! This implementation performs truncation of the value returned by
    ! the HUGE intrinsic *without* doing rounding, which produces the
    ! correct result.
    !
    ! Note that we must also manually check if the emulator is turend on
    ! before performing the truncation (this is normally done by the
    ! overloaded assignment operator, but we are not using it here).
    !

    FUNCTION huge_rpe (a) RESULT (x)
        TYPE(rpe_var), INTENT(IN) :: a
        TYPE(rpe_var) :: x
        INTEGER                    :: lmtb
        INTEGER(KIND=8), PARAMETER :: zero_bits = 0
        INTEGER(KIND=8)            :: bits
        x%sbits = significand_bits(a)
        x%val = HUGE(a%val)
        IF (RPE_ACTIVE) THEN
            IF ((x%sbits == 10) .AND. (RPE_IEEE_HALF)) THEN
                ! For half precision emulation we need to specify the value
                ! explicitly, HUGE cannot do this in the absence of a native
                ! 16-bit real type:
                x%val = 65504
            ELSE
                ! Truncate to the required size without rounding, applying
                ! rounding will always round to infinity and is therefore no
                ! good for this purpose:
                lmtb = 52 - x%sbits - 1
                bits = TRANSFER(x%val, bits)
                CALL MVBITS (zero_bits, 0, lmtb + 1, bits, 0)
                x%val = TRANSFER(bits, x%val)
            END IF
        END IF
    END FUNCTION huge_rpe

    !-------------------------------------------------------------------
    ! Overloaded definitions for (+):
    !
    
    ELEMENTAL FUNCTION add_rpe_complex (x) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var) :: z
        z%sbits = significand_bits(x)
        z = +(x%val)
    END FUNCTION add_rpe_complex

    ELEMENTAL FUNCTION add_rpe_complex_rpe_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y%val
    END FUNCTION add_rpe_complex_rpe_complex

    ELEMENTAL FUNCTION add_rpe_complex_real_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_complex_real_complex

    ELEMENTAL FUNCTION add_rpe_complex_realalt_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val + y
    END FUNCTION add_rpe_complex_realalt_complex

    ELEMENTAL FUNCTION add_real_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_real_complex_rpe_complex

    ELEMENTAL FUNCTION add_realalt_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x + y%val
    END FUNCTION add_realalt_complex_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definitions for (-):
    !

    ELEMENTAL FUNCTION sub_rpe_complex (x) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var) :: z
        z%sbits = significand_bits(x)
        z = -(x%val)
    END FUNCTION sub_rpe_complex

    ELEMENTAL FUNCTION sub_rpe_complex_rpe_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y%val
    END FUNCTION sub_rpe_complex_rpe_complex

    ELEMENTAL FUNCTION sub_rpe_complex_real_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_complex_real_complex

    ELEMENTAL FUNCTION sub_rpe_complex_realalt_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val - y
    END FUNCTION sub_rpe_complex_realalt_complex

    ELEMENTAL FUNCTION sub_real_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_real_complex_rpe_complex

    ELEMENTAL FUNCTION sub_realalt_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x - y%val
    END FUNCTION sub_realalt_complex_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definitions for (*):
    !

    ELEMENTAL FUNCTION mul_rpe_complex_rpe_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y%val
    END FUNCTION mul_rpe_complex_rpe_complex

    ELEMENTAL FUNCTION mul_rpe_complex_real_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_complex_real_complex

    ELEMENTAL FUNCTION mul_rpe_complex_realalt_complex (x, y) RESULT (z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x%val * y
    END FUNCTION mul_rpe_complex_realalt_complex

    ELEMENTAL FUNCTION mul_real_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_REAL_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_real_complex_rpe_complex

    ELEMENTAL FUNCTION mul_realalt_complex_rpe_complex (x, y) RESULT (z)
        COMPLEX(KIND=RPE_ALTERNATE_KIND), INTENT(IN) :: x
        TYPE(rpe_complex_var), INTENT(IN) :: y
        TYPE(rpe_complex_var) :: z
        z%sbits = MAX(significand_bits(x), significand_bits(y))
        z = x * y%val
    END FUNCTION mul_realalt_complex_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definition for CONJG:
    !

    ELEMENTAL FUNCTION conjg_rpe_complex (x) RESULT(z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_complex_var) :: z
        z%sbits = significand_bits(x)
        z = conjg(x%val)
    END FUNCTION conjg_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definition for REAL:
    !

    ELEMENTAL FUNCTION real_rpe_complex (x) RESULT(z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_var) :: z
        z%sbits = significand_bits(x)
        z = real(x%val)
    END FUNCTION real_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definition for REALPART:
    !

    ELEMENTAL FUNCTION realpart_rpe_complex (x) RESULT(z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_var) :: z
        z%sbits = significand_bits(x)
        z = realpart(x%val)
    END FUNCTION realpart_rpe_complex

    !-------------------------------------------------------------------
    ! Overloaded definition for IMAGPART:
    !

    ELEMENTAL FUNCTION imagpart_rpe_complex (x) RESULT(z)
        TYPE(rpe_complex_var), INTENT(IN) :: x
        TYPE(rpe_var) :: z
        z%sbits = significand_bits(x)
        z = imagpart(x%val)
    END FUNCTION imagpart_rpe_complex
