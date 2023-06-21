      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************

      * SET SOURCEFORMAT"FREE"
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Programaflybynight.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT AR-ENTRADA ASSIGN TO "BOOKINGS.DAT"
                 ORGANISATION IS LINE SEQUENTIAL.

       SELECT AR-TRABAJO ASSIGN TO "SORT.TMP".

       SELECT INFORME ASSIGN TO "SUMMARY.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       SELECT AR-ORDENADO ASSIGN TO "BOOKSORT.DAT"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD AR-ENTRADA.
       01  REGISTRO-ENTRADA.
           88 FIN-AR-ENTRADA   VALUE HIGH-VALUE.
           03 NOMBRE           PIC X(30).
           03 DESTINO          PIC X(20).
           03 RESERVA          PIC 9(7).
           03 HOMBRES          PIC 99.
           03 MUJERES          PIC 99.
           03 NINOS            PIC 99.
           03 CATEGORIA        PIC X.

       SD AR-TRABAJO.
       01  REGISTRO-TRABAJO.
           88 FIN-AR-TRABAJO    VALUE HIGH-VALUES.
           03 TNOMBRE           PIC X(30).
           03 TDESTINO          PIC X(20).
           03 TRESERVA          PIC 9(7).
           03 THOMBRES          PIC 99.
           03 TMUJERES          PIC 99.
           03 TNINOS            PIC 99.
           03 TCATEGORIA        PIC X.

       FD INFORME.
       01 LINEA-INFORME        PIC X(48).

       FD AR-ORDENADO.
       01 LINEA-ORDENADO       PIC X(64).

       WORKING-STORAGE SECTION.
       01 DESTINO-ANTERIOR     PIC X(20).

       01 TEMP-LINEA-INFORME.
           03 TEMP-DESTINO         PIC X(20).
           03 TEMP-TOTAL           PIC 9(10).
           03 TEMP-HOMBRES         PIC 9(6).
           03 TEMP-MUJERES         PIC 9(6).
           03 TEMP-NINOS           PIC 9(6).


       PROCEDURE DIVISION.
       ORDENACION-DE-REGISTROS.
           SORT AR-TRABAJO ON ASCENDING TDESTINO
                 INPUT PROCEDURE IS SELECION-TURISMO
                 OUTPUT PROCEDURE IS IMPRIME-INFORME.

           STOP RUN.

       SELECION-TURISMO.
           OPEN INPUT AR-ENTRADA.
           READ AR-ENTRADA
               AT END SET FIN-AR-ENTRADA TO TRUE
           END-READ.

           PERFORM UNTIL FIN-AR-ENTRADA
               IF CATEGORIA EQUALS "T"
      *REVISAR
                   MOVE FUNCTION UPPER-CASE(DESTINO) TO DESTINO
                   RELEASE REGISTRO-TRABAJO FROM REGISTRO-ENTRADA
               END-IF
               READ AR-ENTRADA
                   AT END SET FIN-AR-ENTRADA TO TRUE
               END-READ
           END-PERFORM.

           CLOSE AR-ENTRADA.


       IMPRIME-INFORME.
       OPEN OUTPUT INFORME.
       OPEN OUTPUT AR-ORDENADO.

       RETURN AR-TRABAJO
           AT END SET FIN-AR-TRABAJO TO TRUE
       END-RETURN.

       PERFORM IMPRIME-INFORME-LINEA UNTIL FIN-AR-TRABAJO.

       CLOSE INFORME, AR-ORDENADO.


       IMPRIME-INFORME-LINEA.
           MOVE SPACES TO TEMP-DESTINO.
           MOVE ZEROES TO TEMP-TOTAL.
           MOVE ZEROES TO TEMP-HOMBRES.
           MOVE ZEROES TO TEMP-MUJERES.
           MOVE ZEROES TO TEMP-NINOS.



           MOVE TDESTINO TO TEMP-DESTINO, DESTINO-ANTERIOR.

           PERFORM UNTIL TDESTINO NOT = DESTINO-ANTERIOR

               ADD TRESERVA TO TEMP-TOTAL
               ADD THOMBRES TO TEMP-HOMBRES
               ADD TMUJERES TO TEMP-MUJERES
               ADD TNINOS   TO TEMP-NINOS

               WRITE LINEA-ORDENADO FROM REGISTRO-TRABAJO

               RETURN AR-TRABAJO
                   AT END SET FIN-AR-TRABAJO TO TRUE
               END-RETURN

           END-PERFORM.

           EVALUATE TEMP-DESTINO

               WHEN "AFGHANISTAN"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.50
               WHEN "CAMBODIA"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.24
               WHEN "CORSICA"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.18
               WHEN "EL SALVADOR"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.85
               WHEN "HAITI"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.21
               WHEN "HONDURAS"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.23
               WHEN "ISRAEL"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.11
               WHEN "IRAN"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.57
               WHEN "IRAQ"
                 COMPUTE TEMP-TOTAL = TEMP-TOTAL * 1.33

           END-EVALUATE.

           WRITE LINEA-INFORME FROM TEMP-LINEA-INFORME.
