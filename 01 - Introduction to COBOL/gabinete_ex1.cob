      *DESCRIPTION: This program computes for the factorial of a number n.
      *AUTHOR: Keith Ginoel S. Gabinete
      *DATE: August 28, 2024 
       IDENTIFICATION DIVISION.
       PROGRAM-ID. gabinete_ex1.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 NUM-REC.
               02 NUM-ARR PIC 9(1) OCCURS 5 TIMES.
           
           77 IDX PIC 99 VALUE 1.

       PROCEDURE DIVISION.
      *COBOL indexing starts with 1
           MOVE "Hello" TO MY-ARR(1).
           MOVE "World" TO MY-ARR(10).

           PERFORM 10 times
               DISPLAY ARR-REC(IDX)

               DISPLAY IDX " " MY-ARR(IDX)
               COMPUTE IDX = IDX + 1
      *this is also valid for incrementing IDX: ADD 1 to IDX
           END-PERFORM.

           STOP RUN.
        