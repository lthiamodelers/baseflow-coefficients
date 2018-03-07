************************************************************************
*
*     Subroutine JROGER                       Called by: 
*
*     print a skull and cross bones (Jolly Roger) to alert the user to
*     an important warning message.
*
*     ASCII version of Jolly Roger courtesy of Joan Stark
*     (http://www.geocities.com/spunk1111/).  I emailed her
*     (spunk1111@juno.com) on Jan 23, 2013, but the address is no longer
*     valid.
*
*     All usage of '\' needs to be escaped with a \
*
************************************************************************
      SUBROUTINE JROGER(LDNUM)
*
*     subroutine arguments
*
      INTEGER*4 LDNUM
*
*     write heading to file
*
      WRITE(LDNUM,*)
      WRITE(LDNUM,*) '.-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--.'
      WRITE(LDNUM,*) '|                W A R N I N G ! !             |'
      WRITE(LDNUM,*) '|                    ______                    |'
      WRITE(LDNUM,*) '|                 .-"      "-.                 |'
      WRITE(LDNUM,*) '|                /            \\                |'
      WRITE(LDNUM,*) '|    _          |              |          _    |'
      WRITE(LDNUM,*) '|   ( \\         |,  .-.  .-.  ,|         / )   |'
      WRITE(LDNUM,*) '|    > "=._     | )(__/  \\__)( |     _.=" <    |'
      WRITE(LDNUM,*)
     &     '|   (_/"=._"=._ |/     /\\     \\| _.="_.="\\_)   |'
      WRITE(LDNUM,*) '|          "=._"(_     ^^     _)"_.="          |'
      WRITE(LDNUM,*) '|              "=\\__|IIIIII|__/="              |'
      WRITE(LDNUM,*) '|             _.="| \\IIIIII/ |"=._             |'
      WRITE(LDNUM,*) '|   _     _.="_.="\\          /"=._"=._     _   |'
      WRITE(LDNUM,*) '|  ( \\_.="_.="     `--------`     "=._"=._/ )  |'
      WRITE(LDNUM,*) '|   > _.="                            "=._ <   |'
      WRITE(LDNUM,*) '|  (_/   jgs                              \\_)  |'
      WRITE(LDNUM,*) '|                W A R N I N G ! !             |'
      WRITE(LDNUM,*) '|                                              |'
      WRITE(LDNUM,*) '.=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-.'
      WRITE(LDNUM,*)

      RETURN
      END
