    PUBLIC :: huge
    INTERFACE huge
        MODULE PROCEDURE huge_rpe
    END INTERFACE huge

    INTERFACE OPERATOR(+)
        MODULE PROCEDURE add_rpe_complex
        MODULE PROCEDURE add_rpe_complex_rpe_complex
        MODULE PROCEDURE add_rpe_complex_real_complex
        MODULE PROCEDURE add_rpe_complex_realalt_complex
        MODULE PROCEDURE add_real_complex_rpe_complex
        MODULE PROCEDURE add_realalt_complex_rpe_complex
    END INTERFACE OPERATOR(+)

    INTERFACE OPERATOR(-)
        MODULE PROCEDURE sub_rpe_complex
        MODULE PROCEDURE sub_rpe_complex_rpe_complex
        MODULE PROCEDURE sub_rpe_complex_real_complex
        MODULE PROCEDURE sub_rpe_complex_realalt_complex
        MODULE PROCEDURE sub_real_complex_rpe_complex
        MODULE PROCEDURE sub_realalt_complex_rpe_complex
    END INTERFACE OPERATOR(-)

    INTERFACE OPERATOR(*)
        MODULE PROCEDURE mul_rpe_complex_rpe_complex
        MODULE PROCEDURE mul_rpe_complex_real_complex
        MODULE PROCEDURE mul_rpe_complex_realalt_complex
        MODULE PROCEDURE mul_real_complex_rpe_complex
        MODULE PROCEDURE mul_realalt_complex_rpe_complex
        MODULE PROCEDURE mul_rpe_complex_real
        MODULE PROCEDURE mul_rpe_complex_realalt
        MODULE PROCEDURE mul_real_rpe_complex
        MODULE PROCEDURE mul_realalt_rpe_complex
        MODULE PROCEDURE mul_rpe_complex_rpe
        MODULE PROCEDURE mul_rpe_rpe_complex
    END INTERFACE OPERATOR(*)

    PUBLIC :: CONJG
    INTERFACE CONJG
        MODULE PROCEDURE conjg_rpe_complex
    END INTERFACE CONJG

    PUBLIC :: REAL
    INTERFACE REAL
        MODULE PROCEDURE real_rpe_complex
    END INTERFACE REAL

    PUBLIC :: REALPART
    INTERFACE REALPART
        MODULE PROCEDURE realpart_rpe_complex
    END INTERFACE REALPART

    PUBLIC :: IMAGPART
    INTERFACE IMAGPART
        MODULE PROCEDURE imagpart_rpe_complex
    END INTERFACE IMAGPART

    PUBLIC :: CMPLX
    INTERFACE CMPLX
        MODULE PROCEDURE cmplx_rpe_var
    END INTERFACE CMPLX
