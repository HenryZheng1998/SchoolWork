       identification division.
       program-id. Program2.
       author. Henry Zheng. 
       date-written. 2018-04-15.
      * Purpose: Process the valid data and split the records into 2
      *          files and count

       environment division.
       configuration section.

       input-output section.
       file-control.
      *configure input file
           select input-file
               assign to "../../../data2/valid.out"
               organization is line sequential.

      * configure output file
           select SL-RECORDS-DATA-FILE 
               assign to "../../../data2/SL.out"
               organization is line sequential.
      * configure output file
      *    select RETURN-RECORDS-DATA-FILE 
      *        assign to "../../../data2/returns.out"
      *        organization is line sequential.

       data division.
       file section.   
      * declare an input record definition
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code           pic x.
           05 il-transaction-amount         pic 9(5)V99.
           05 il-payment-type               pic xx.
           05 il-store-number               pic 99.
           05 il-invoice-number.
               10 invoice-number-1          pic xx.
               10 invoice-number-2          pic x.
               10 invoice-number-3          pic 9(6).
           05 il-SKU-code                   pic x(15).

      * declare an output record definition
       fd SL-RECORDS-DATA-FILE
           data record is SL-line
           record contains 120 characters.

       01 SL-line                        pic x(120).

      *fd RETURN-RECORDS-DATA-FILE
      *    data record is Return-line
      *    record contains 36 characters.
      *
      *01 Return-line                        pic x(79).
       
       working-storage section.
      * General storage section
        
      * end of file flag 
       77  ws-eof                          pic x
           value "N".

       01 ws-grand-total                   pic 9(6)V99
           value 0.

      * SL storage section
       01 ws-sale-count                    pic 999
           value 0.
       01 ws-sale-tamount                 pic 9(6)V99
           value 0.
       01 ws-layaway-count                 pic 999
           value 0.
       01 ws-layaway-tamount                 pic 9(6)V99
           value 0.
       01 ws-SL-store-01-tamount                 pic 9(6)V99
           value 0.
       01 ws-SL-store-02-tamount                 pic 9(6)V99
           value 0.
       01 ws-SL-store-03-tamount                 pic 9(6)V99
           value 0.
       01 ws-SL-store-07-tamount                 pic 9(6)V99
           value 0.

      * Return storage section
       01 ws-return-count                      pic 999
           value 0.
       01 ws-return-tamount                    pic 9(6)V99
           value 0.

       01 ws-R-store-01-tamount                 pic 9(6)V99
           value 0.
       01 ws-R-store-02-tamount                 pic 9(6)V99
           value 0.
       01 ws-R-store-03-tamount                 pic 9(6)V99
           value 0.
       01 ws-R-store-07-tamount                 pic 9(6)V99
           value 0.

      * Output storage section
       01 ws-newline                               pic x
           value spaces.
       01 ws-headline.
           05  filler                              pic x(20)
               value "Sale & Layway Report".
       
       01 ws-S-columns.
           05  filler                               pic x(20)
               value "Total Sale records  ".
           05  filler                               pic x(19)
               value "Total Sale amount  ".

       01 ws-L-columns.
           05  filler                               pic x(23)
               value "Total Layaway records  ".
           05  filler                               pic x(21)
               value "Total Layway amount  ".


       01 ws-S-output.
           05  ol-S-record                         pic xxx.
           05  filler                              pic x(17)
               value spaces.
           05  ol-S-total                          pic zzz,zz9.99.
           05  filler                              pic x(11)
               value spaces.

       01 ws-L-output.
           05  ol-L-record                         pic xxx.
           05  filler                              pic x(20)
               value spaces.
           05  ol-L-total                          pic zzz,zz9.99.


       01 ws-SL-total.
           05  filler                              pic x(29)
               value "Total Sale and Layway record:".
           05  ws-SL-combine-total                 pic 999.
           05  filler                              pic xxx
               value spaces.
           05  filler                              pic x(29)
               value "Total Sale and Layway amount:".
           05  ws-SL-tamount                       pic zzz,zz9.99.

       01 ws-S-perc-output.
           05  filler                              pic x(17)
               value "Sale Percentage: ".
           05  ws-S-perc                           pic Z9.99.
           05  filler                              pic x
               value "%".

       01 ws-L-perc-output.
           05  filler                              pic x(20)
               value "Layaway Percentage: ".
           05  ws-L-perc                           pic Z9.99.
           05  filler                              pic x
               value "%".

       01 ws-SL-store-head.
           05  filler                              pic x(24)
               value "Total Transaction Stores".

       01 ws-SL-store-column.
           05  filler                              pic x(10)
               value "Store-01  ".
           05  filler                              pic x(10)
               value "Store-02  ".
           05  filler                              pic x(10)
               value "Store-03  ".
           05  filler                              pic x(10)
               value "Store-07  ".

       01 ws-SL-Store-output.
           05  ol-store-01                         pic zzz,zz9.99.
           05  filler                              pic xx
               value spaces.
           05  ol-store-02                         pic zzz,zz9.99.
           05  filler                              pic xx
               value spaces.
           05  ol-store-03                         pic zzz,zz9.99.
           05  filler                              pic xx
               value spaces.
           05  ol-store-07                         pic zzz,zz9.99.
           05  filler                              pic xx
               value spaces.

       procedure division.
           open input input-file,
           output SL-RECORDS-DATA-FILE.
      *        RETURN-RECORDS-DATA-FILE.

           read input-file
               at end move "Y"              to ws-eof.
           perform until ws-eof equals "Y"

               perform 100-check-SL-or-R
           
           read input-file
                   at end move "Y"     to ws-eof  
           end-perform.

           perform 210-calc-and-move-SL-totals.

           write SL-line                   from ws-headline.
           write SL-line                   from ws-newline.
           write SL-line                   from ws-S-columns.
           write SL-line                   from ws-newline.
           write SL-line                   from ws-S-output.
           write SL-line                   from ws-newline.
           write SL-line                   from ws-S-columns.
           write SL-line                   from ws-newline.
           write SL-line                   from ws-S-output.
           write SL-line                   from ws-SL-total.
           write SL-line                   from ws-newline.
           write SL-line                   from ws-SL-store-head.
           write SL-line                   from ws-SL-store-column.
           write SL-line                   from ws-SL-Store-output.
           write SL-line                   from ws-S-perc-output.
          
           close input-file SL-RECORDS-DATA-FILE.
      *            RETURN-RECORDS-DATA-FILE.

           goback.

       100-check-SL-or-R.
           if (il-transaction-code = "S" OR "L") then
               perform 110-SL-count-and-add
           else
               perform 120-R-count-and-add
           end-if.

       110-SL-count-and-add.
           if (il-transaction-code = "S") then
               add 1 to ws-sale-count
               add il-transaction-amount to ws-sale-tamount
           end-if.

           if (il-transaction-code = "L") then
               add 1 to ws-layaway-count
               add il-transaction-amount to ws-layaway-tamount
           end-if.

           if (il-store-number = 01) then
               add il-transaction-amount to ws-SL-store-01-tamount
           end-if.

           if (il-store-number = 02) then
               add il-transaction-amount to ws-SL-store-02-tamount
           end-if.

           if (il-store-number = 03) then
               add il-transaction-amount to ws-SL-store-03-tamount
           end-if.

           if (il-store-number = 07) then
               add il-transaction-amount to ws-SL-store-07-tamount
           end-if.
       
       120-R-count-and-add.
           add 1 to ws-return-count
           add il-transaction-amount to ws-return-tamount

           if (il-store-number = 01) then
               add il-transaction-amount to ws-R-store-01-tamount
           end-if.

           if (il-store-number = 02) then
               add il-transaction-amount to ws-R-store-02-tamount
           end-if.

           if (il-store-number = 03) then
               add il-transaction-amount to ws-R-store-03-tamount
           end-if.

           if (il-store-number = 07) then
               add il-transaction-amount to ws-R-store-07-tamount
           end-if.

       210-calc-and-move-SL-totals.
           move ws-sale-count          to ol-S-record.
           move ws-sale-tamount        to ol-S-total.
           move ws-layaway-count       to ol-L-record.
           move ws-layaway-tamount     to ol-L-total.
           move ws-SL-store-01-tamount to ol-store-01.
           move ws-SL-store-02-tamount to ol-store-02.
           move ws-SL-store-03-tamount to ol-store-03.
           move ws-SL-store-07-tamount to ol-store-07.

           compute ws-SL-combine-total =
             (ws-sale-count + ws-layaway-count).

           compute ws-SL-tamount =
             (ws-sale-tamount + ws-layaway-tamount).

           compute ws-S-perc =
             (ws-sale-count / ws-SL-combine-total).
       end program Program2.