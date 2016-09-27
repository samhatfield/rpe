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
