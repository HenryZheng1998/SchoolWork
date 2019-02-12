       identification division.
       program-id. Edit.
       author. Paul Kerrigan. 
       date-written. 2018-04-07.
      * Purpose: To do validation on all of the input

       environment division.
       configuration section.

       input-output section.
       file-control.
      *configure input file
           select input-file
               assign to "../../../../datafiles/data/project1.dat"
               organization is line sequential.

      * configure output file
           select VALID-RECORDS-DATA-FILE 
               assign to "../../../../datafiles/data/valid.out"
               organization is line sequential.
      * configure output file
           select INVALID-RECORDS-DATA-FILE 
               assign to "../../../../datafiles/data/invalid.out"
               organization is line sequential.
      * configure output file
           select ERROR-RECORDS-DATA-FILE 
               assign to "../../../../datafiles/data/error.out"
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
               10 invoice-number-2          pic x.
                   88 il-test
                       value '-'.
               10 invoice-number-3          pic 9(6).
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

       fd ERROR-RECORDS-DATA-FILE
           data record is error-line
           record contains 200 characters.

       01 error-line                        pic x(200).

       working-storage section.

       

       01 ws-detail-line.
           05 filler                               pic x(5)
               value spaces.
           05 ws-ol-transaction-code               pic x.
           05 filler                               pic x(8)
               value spaces.
           05 ws-ol-transaction-amount             pic $$$$,$$9.99.
           05 filler                               pic x(4)
               value spaces.
           05 ws-ol-payment-type                   pic xx.
           05 filler                               pic x(7)
               value spaces.
           05 ws-ol-store-number                   pic z9.
           05 filler                               pic x(4)
               value spaces.
           05 ws-ol-invoice-number                 pic x(9).
           05 filler                               pic x(4)
               value spaces.
           05 ws-ol-sku-code                       pic x(15).
           05 filler                               pic xxx
               value spaces.
           05 ws-message                           pic x(12).
           05 filler                               pic x(2)
                value spaces.
           05 ws-message1                          pic x(18).
           05 filler                               pic x(2)
                value spaces.
           05 ws-message2                          pic x(15).
           05 filler                               pic x(2)
                value spaces.
           05 ws-message3                          pic x(15).
           05 filler                               pic x(2)
                value spaces.
           05 ws-message4                          pic x(20).
           05 filler                               pic x(2)
                value spaces.
           05 ws-message5                          pic x(12).
           05 filler                               pic x(2)
                value spaces.

      *heading outputs
        01  ws-heading-line.
           05  filler                       pic x(20)
               value spaces.
           05  filler                       pic x(12)
               value "ERROR REPORT". 

       01 ws-heading-line2.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(11)
               value "TRANSACTION".
           05 filler                               pic xx
               value spaces.
           05 filler                               pic x(11)
               value "TRANSACTION".        
           05 filler                               pic xx
               value spaces.
           05 filler                               pic x(7)
               value "PAYMENT".
           05 filler                               pic xx
               value spaces.
           05 filler                               pic x(5)
               value "STORE".
           05 filler                               pic xxx
               value spaces.       
           05 filler                               pic x(7)
               value "INVOICE".
           05 filler                               pic x(10)
               value spaces.
           05 filler                               pic x(3)
               value "SKU".
           05 filler                               pic x(50)
               value spaces.
           05 filler                               pic x(6)
               value "ERRORS".
           


       01 ws-heading-line3.
           05  filler                              pic x(4)
               value spaces.
           05  filler                              pic x(4)
               value "CODE".
           05  filler                              pic x(8)
               value spaces.
           05  filler                              pic x(6)
               value "AMOUNT".
           05  filler                              pic x(5)
               value spaces.
           05  filler                              pic x(4)
               value "TYPE".
           05  filler                              pic x(5)
               value spaces.
           05  filler                              pic x(6)
               value "NUMBER".
           05  filler                              pic xx
               value spaces.
           05  filler                              pic x(6)
               value "NUMBER".
           05  filler                              pic x(11)
               value spaces.
           05  filler                              pic x(4)
               value "CODE".
           


       01 ws-underlines.
           05  filler                              pic x
               value spaces.
           05  filler                              pic x(11)
               value "-----------".
           05  filler                              pic xx
               value spaces.
           05  filler                              pic x(11)
               value "-----------".
           05  filler                              pic xx
               value spaces.
           05  filler                              pic x(7)
               value "-------".
           05  filler                              pic xx
               value spaces.
           05  filler                              pic x(6)
               value "------".
           05  filler                              pic xx
               value spaces.
           05  filler                              pic x(7)
               value "-------".
           05  filler                              pic x(10)
               value spaces.
           05  filler                              pic x(4)
               value "----".
           05  filler                              pic x(13).
           
       
      * end of file flag 
        77  ws-eof                          pic x
           value "N".
        77 ws-valid-flag                    pic x
           value "N".
       procedure division.

      *opens both input and output files
           open input input-file,
                output VALID-RECORDS-DATA-FILE
                   INVALID-RECORDS-DATA-FILE ERROR-RECORDS-DATA-FILE.

      * read initial record from input-file
           read input-file at end move "Y" to ws-eof.

           write error-line               from ws-heading-line.
           move spaces                    to error-line.
           write error-line.
           write error-line               from ws-heading-line2.
           write error-line               from ws-heading-line3.
           write error-line               from ws-underlines.
           move spaces                    to error-line.
           write error-line.

      * iterate through all input lines        
           perform 20-process-lines until ws-eof = "Y".

           close input-file VALID-RECORDS-DATA-FILE
                   INVALID-RECORDS-DATA-FILE ERROR-RECORDS-DATA-FILE.
           goback.

       20-process-lines.

           move il-transaction-code         to ws-ol-transaction-code.
           move il-transaction-amount       to ws-ol-transaction-amount.
           move il-payment-type             to ws-ol-payment-type.
           move il-store-number             to ws-ol-store-number.
           move il-invoice-number           to ws-ol-invoice-number.
           move il-sku-code                 to ws-ol-sku-code.

           

           move "Y" to ws-valid-flag.

           if not il-transaction-valid then
               move "N" to ws-valid-flag
               move "invalid code" to ws-message
              
           end-if.

           if il-transaction-amount is not numeric then
               move "N" to ws-valid-flag
               move "Amount not numeric" to ws-message1 
              
           end-if

           if not il-payment-valid then
               move "N" to ws-valid-flag
               move "invalid payment" to ws-message2 
               
           end-if.

           if not il-store-valid then
               move "N" to ws-valid-flag
               move "invalid store #" to ws-message3
               
           end-if.

           if invoice-number-1 is not alphabetic then
              
               move "N" to ws-valid-flag
               move "not alpha or numeric" to ws-message4 
            

           else
               if not il-test then
               move "N" to ws-valid-flag
               move "not alpha or numeric" to ws-message4
               
           else
               
               if invoice-number-3 is not numeric then
               move "N" to ws-valid-flag
               move "not alpha or numeric" to ws-message4
               
           end-if
           end-if
           end-if.
           if NOT il-SKU-code NOT = SPACE AND LOW-VALUE then
               move "N" to ws-valid-flag
               move "invalid SKU" to ws-message5
               
           end-if.

           

           IF (ws-valid-flag is equal to "N")then
               move spaces                to invalid-line
               write invalid-line         from input-line
      *        write error-line           from ws-error
               write error-line           from ws-detail-line
           else
               move spaces                to valid-line
               write valid-line           from input-line
           END-IF.

           
          move spaces to ws-message.
          move spaces to ws-message1.
          move spaces to ws-message2.
          move spaces to ws-message3.
          move spaces to ws-message4.
          move spaces to ws-message5.
           
          
           

      * read next input-file record
           read input-file
               at end move "Y"              to ws-eof.

       end program Edit.