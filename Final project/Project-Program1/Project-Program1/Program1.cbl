       identification division.
       program-id. Program1.
       author. Paul Kerrigan. 
       date-written. 2018-04-07.
      * Purpose: Demonstarte how to use paragraphs and have
      * them structured properly

       environment division.
       configuration section.

       input-output section.
       file-control.
      *configure input file
           select input-file
               assign to "../../../data/project1.dat"
               organization is line sequential.

      * configure output file
           select VALID-RECORDS-DATA-FILE 
               assign to "../../../data/valid.out"
               organization is line sequential.
      * configure output file
           select INVALID-RECORDS-DATA-FILE 
               assign to "../../../data/invalid.out"
               organization is line sequential.

       data division.
       file section.
      * declare an input record definition
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code           pic x.
               88 il-transaction-valid
                  value 'S', 'R', 'L'.
           05 il-transaction-amount         pic 9(5)V99.
           05 il-payment-type               pic xx.
               88 il-payment-valid
                  value 'CR', 'DB', 'CA'.
           05 il-store-number               pic 99.
               88 il-store-valid
                  value 01, 02, 03, 07.
           05 il-invoice-number.
               10 invoice-number-1          pic xx.
                   88 il-alpha
                       value 'A' thru 'Z'.
               10 invoice-number-2          pic x.
               10 invoice-number-3          pic 9(6).
                   88 il-numeric
                       value 0 thru 9.
           05 il-SKU-code                   pic x(15).

      * declare an output record definition
       fd VALID-RECORDS-DATA-FILE
           data record is valid-line
           record contains 36 characters.

       01 valid-line                        pic x(79).
             

       fd INVALID-RECORDS-DATA-FILE
           data record is invalid-line
           record contains 36 characters.

       01 invalid-line                      pic x(79).

       working-storage section.
       
      * end of file flag 
        77  ws-eof                          pic x
           value "N".
        77 ws-valid-flag                    pic x
           value "N".
       procedure division.

      *opens both input and output files
           open input input-file,
                output VALID-RECORDS-DATA-FILE
                   INVALID-RECORDS-DATA-FILE.

      * read initial record from input-file
           read input-file at end move "Y" to ws-eof.

      * iterate through all input lines        
           perform 20-process-lines until ws-eof = "Y".

           close input-file VALID-RECORDS-DATA-FILE
                   INVALID-RECORDS-DATA-FILE.
           goback.

       20-process-lines.
           move "Y" to ws-valid-flag.

           if not il-transaction-valid then
               move "N" to ws-valid-flag
           end-if.

           if not il-payment-valid then
               move "N" to ws-valid-flag
           end-if.

           if not il-store-valid then
               move "N" to ws-valid-flag
           end-if.

           if not il-alpha and not il-numeric then
               move "N" to ws-valid-flag
           end-if.

           if NOT il-SKU-code NOT = SPACE AND LOW-VALUE then
               move "N" to ws-valid-flag
           end-if.

           IF (ws-valid-flag is equal to "N")then
               move spaces                to invalid-line
               write invalid-line         from input-line
           else
               move spaces                to valid-line
               write valid-line           from input-line
           END-IF

           
           
           
          
           

      * read next input-file record
           read input-file
               at end move "Y"              to ws-eof.

       end program Program1.