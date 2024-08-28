      *DESCRIPTION: 
      *AUTHOR: Keith Ginoel S. Gabinete
      *DATE: August 28, 2024 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. array.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ARR-REC.
               02 MY-ARR PIC A(10) OCCURS 10 TIMES.    
       77 IDX PIC 99 VALUE 1.

       PROCEDURE DIVISION.
      *COBOL indexing starts with 1
           MOVE "Hello" TO MY-ARR(1).
           MOVE "World" TO MY-ARR(10).

           PERFORM 10 times
               DISPLAY IDX " " MY-ARR(IDX)
               COMPUTE IDX = IDX + 1
      *this is also valid for incrementing IDX: ADD 1 to IDX
           END-PERFORM.

           STOP RUN.
        