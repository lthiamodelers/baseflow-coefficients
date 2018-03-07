************************************************************************
*
*     Function GETINT              Called by: AMLLOAD, LADLOAD, MLELOAD,
*                                             SETXL2
*
*     convert all or part of a character variable to an integer.
*
*     This function seems trivial, but it allows one to access
*     individual characters within the string variable when the
*     string variable is a vector in the calling program (i.e. it
*     doens't seem possible to use the ':' notation when the character
*     variable is a vector - we get around this by passing a single
*     element of the vector to this function).
*
************************************************************************
      INTEGER*4 FUNCTION GETINT(STRVAR,ISTART,IEND)
*
*     function arguments
*
      CHARACTER*(*) STRVAR
      INTEGER*4 ISTART,IEND
      READ(STRVAR(ISTART:IEND),*) GETINT

      RETURN
      END

