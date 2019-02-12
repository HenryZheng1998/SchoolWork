       identification division.
       program-id. DataSplit.
       author. Henry Zheng Paul Kerrigan. 
       date-written. 2018-04-15.
      * Purpose: Process the valid data and split the records into 2
      *          files and count

       environment division.
       configuration section.

       input-output section.
       file-control.
      *configure input file
           select input-file
               assign to "../../../../datafiles/data/valid.out"
               organization is line sequential.

      * configure output file
           select SL-RECORDS-DATA-FILE 
               assign to "../../../../datafiles/data/SL.out"
               organization is line sequential.

      * configure output file
           select RETURN-RECORDS-DATA-FILE 
               assign to "../../../../datafiles/data/returns.out"
               organization is line sequential.

      * configure output file
           select COUNTS-RECORDS-FILE 
               assign to "../../../../datafiles/data/counts.out"
               organization is line sequential.

       data division.
       file section.   
      * declare an input record definition
       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code              pic x.
           05 il-transaction-amount            pic 9(5)V99.
           05 il-payment-type                  pic xx.
           05 il-store-number                  pic 99.
           05 il-invoice-number.
               10 invoice-number-1             pic xx.
               10 invoice-number-2             pic x.
               10 invoice-number-3             pic 9(6).
           05 il-SKU-code                      pic x(15).

      * declare an output record definition
       fd SL-RECORDS-DATA-FILE
           data record is SL-line
           record contains 80 characters.

       01 SL-line                              pic x(80).

       fd RETURN-RECORDS-DATA-FILE
           data record is R-line
           record contains 36 characters.
       
       01 R-line                               pic x(80).
       
       fd COUNTS-RECORDS-FILE
           data record is C-Line
           record contains 80 characters.

       01 C-Line                               pic x(80).
       working-storage section.
      * General storage section
        
      * end of file flag 
       77  ws-eof                              pic x
           value "N".

       77 ws-newline                           pic x
           value spaces.

      * SL storage section
       01 ws-sale-count                        pic 999
           value 0.
       01 ws-sale-tamount                      pic 9(6)V99
           value 0.
       01 ws-layaway-count                     pic 999
           value 0.
       01 ws-layaway-tamount                   pic 9(6)V99
           value 0.
       01 ws-SL-store-01-tamount               pic 9(6)V99
           value 0.
       01 ws-SL-store-02-tamount               pic 9(6)V99
           value 0.
       01 ws-SL-store-03-tamount               pic 9(6)V99
           value 0.
       01 ws-SL-store-07-tamount               pic 9(6)V99
           value 0.

       01 ws-SL-tamount                        pic 9(6)V99
           value 0.
      * Return storage section
       01 ws-return-count                      pic 999
           value 0.
       01 ws-return-tamount                    pic 9(6)V99
           value 0.

       01 ws-R-store-01-tamount                pic 9(6)V99
           value 0.
       01 ws-R-store-02-tamount                pic 9(6)V99
           value 0.
       01 ws-R-store-03-tamount                pic 9(6)V99
           value 0.
       01 ws-R-store-07-tamount                pic 9(6)V99
           value 0.

      * SL output storage section
       01 ws-SL-headline.
           05  filler                          pic x(21)
               value spaces.
           05  filler                          pic x(20)
               value "Sale & Layway Report".
       
       01 ws-S-columns.
           05  filler                          pic x(18)
               value "Total Sale records".
           05  filler                          pic x(5)
               value spaces.
           05  filler                          pic x(17)
               value "Total Sale amount".
           05  filler                          pic x(4)
               value spaces.
           05  filler                          pic x(17)
               value "Sale Percentage: ".

       01 ws-L-columns.
           05  filler                          pic x(21)
               value "Total Layaway records".
           05  filler                          pic xx
               value spaces.
           05  filler                          pic x(19)
               value "Total Layway amount".
           05  filler                          pic xx
               value spaces.
           05  filler                          pic x(20)
               value "Layaway Percentage: ".

       01 ws-S-output.
           05  filler                          pic x(7)
               value spaces.
           05  ol-S-record                     pic zzz.
           05  filler                          pic x(20)
               value spaces.
           05  ol-S-total                      pic $$$,$$9.99.
           05  filler                          pic x(4)
               value spaces.
           05  ws-S-perc                       pic Z9.99.
           05  filler                          pic x
               value "%".

       01 ws-L-output.
           05  filler                          pic x(7)
               value spaces.
           05  ol-L-record                     pic zzz.
           05  filler                          pic x(20)
               value spaces.
           05  ol-L-total                      pic $$$,$$9.99.
           05  filler                          pic x(4)
               value spaces.
           05  ws-L-perc                       pic Z9.99.
           05  filler                          pic x
               value "%".

       01 ws-SL-total.
           05  filler                          pic x(30)
               value "Total Sale and Layway record: ".
           05  ws-SL-combine-total             pic 999.
           05  filler                          pic xxx
               value spaces.
           05  filler                          pic x(30)
               value "Total Sale and Layway amount: ".
           05  ws-SL-tamount-final             pic $$$,$$9.99.

       01 ws-SL-store-head.
           05  filler                          pic x(24)
               value "Total Transaction Stores".

       01 ws-SL-store-01.
           05  filler                          pic x(10)
               value "Store-01: ".
           05  ol-SL-store-01                  pic $$$,$$9.99.

       01 ws-SL-store-02.
           05  filler                          pic x(10)
               value "Store-02: ".
           05  ol-SL-store-02                  pic $$$,$$9.99.

       01 ws-SL-store-03.
           05  filler                          pic x(10)
               value "Store-03: ".
           05  ol-SL-store-03                  pic $$$,$$9.99.

       01 ws-SL-store-07.
           05  filler                          pic x(10)
               value "Store-07: ".
           05  ol-SL-store-07                  pic $$$,$$9.99.

      * R output storage section
       01 ws-R-headline.
           05  filler                          pic x(21)
               value spaces.
           05  filler                          pic x(14)
               value "Returns Report".

       01 ws-R-output.
           05  filler                          pic x(7)
               value spaces.
           05  ol-R-record                     pic zzz.
           05  filler                          pic x(20)
               value spaces.
           05  ol-R-total                      pic $$$,$$9.99.

       01 ws-R-columns.
           05  filler                          pic x(21)
               value "Total returns records".
           05  filler                          pic x(5)
               value spaces.
           05  filler                          pic x(20)
               value "Total returns amount".

       01 ws-R-store-head.
           05  filler                          pic x(19)
               value "Total Return Stores".

       01 ws-R-store-01.
           05  filler                          pic x(10)
               value "Store-01: ".
           05  ol-R-store-01                   pic $$$,$$9.99.

       01 ws-R-store-02.
           05  filler                          pic x(10)
               value "Store-02: ".
           05  ol-R-store-02                   pic $$$,$$9.99.

       01 ws-R-store-03.
           05  filler                          pic x(10)
               value "Store-03: ".
           05  ol-R-store-03                   pic $$$,$$9.99.

       01 ws-R-store-07.
           05  filler                          pic x(10)
               value "Store-07: ".
           05  ol-R-store-07                   pic $$$,$$9.99.

       01 ws-R-grand-total.
           05  filler                          pic x(20)
               value "Grand Total Amount: ".
           05  ws-R-calc-total                 pic $$$,$$9.99
               value 0.
       procedure division.
           open input input-file,
           output SL-RECORDS-DATA-FILE
               RETURN-RECORDS-DATA-FILE COUNTS-RECORDS-FILE.

           read input-file
               at end move "Y"                 to ws-eof.
           perform until ws-eof equals "Y"

               perform 100-check-SL-or-R
           
           read input-file
                   at end move "Y"             to ws-eof  
           end-perform.

           perform 210-calc-and-move-SL-totals.
           perform 230-write-SL.

           perform 220-calc-and-move-R-totals.
           perform 240-wrote-R.
          
           close input-file SL-RECORDS-DATA-FILE
                   RETURN-RECORDS-DATA-FILE COUNTS-RECORDS-FILE.

           goback.

       100-check-SL-or-R.
           if (il-transaction-code = "S" OR "L") then
               perform 110-SL-count-and-add
               write SL-Line                    from input-line
           else
               perform 120-R-count-and-add
               write R-line                     from input-line
           end-if.

       110-SL-count-and-add.
           if (il-transaction-code = "S") then
               add 1 to ws-sale-count
               add il-transaction-amount       to ws-sale-tamount
           end-if.

           if (il-transaction-code = "L") then
               add 1 to ws-layaway-count
               add il-transaction-amount       to ws-layaway-tamount
           end-if.

           if (il-store-number = 01) then
               add il-transaction-amount       to ws-SL-store-01-tamount
           end-if.

           if (il-store-number = 02) then
               add il-transaction-amount       to ws-SL-store-02-tamount
           end-if.

           if (il-store-number = 03) then
               add il-transaction-amount       to ws-SL-store-03-tamount
           end-if.

           if (il-store-number = 07) then
               add il-transaction-amount       to ws-SL-store-07-tamount
           end-if.
       
       120-R-count-and-add.
           add 1 to ws-return-count
           add il-transaction-amount           to ws-return-tamount

           if (il-store-number = 01) then
               add il-transaction-amount       to ws-R-store-01-tamount
           end-if.

           if (il-store-number = 02) then
               add il-transaction-amount       to ws-R-store-02-tamount
           end-if.

           if (il-store-number = 03) then
               add il-transaction-amount       to ws-R-store-03-tamount
           end-if.

           if (il-store-number = 07) then
               add il-transaction-amount       to ws-R-store-07-tamount
           end-if.

       210-calc-and-move-SL-totals.
           move ws-sale-count                  to ol-S-record.
           move ws-sale-tamount                to ol-S-total.
           move ws-layaway-count               to ol-L-record.
           move ws-layaway-tamount             to ol-L-total.
           move ws-SL-store-01-tamount         to ol-SL-store-01.
           move ws-SL-store-02-tamount         to ol-SL-store-02.
           move ws-SL-store-03-tamount         to ol-SL-store-03.
           move ws-SL-store-07-tamount         to ol-SL-store-07.

           compute ws-SL-combine-total =
             (ws-sale-count + ws-layaway-count).

           compute ws-SL-tamount =
             (ws-sale-tamount + ws-layaway-tamount).

           compute ws-S-perc rounded =
             ((ws-sale-count / ws-SL-combine-total)*100).

           compute ws-L-perc rounded =
             ((ws-layaway-count / ws-SL-combine-total)*100).

           move ws-SL-tamount                  to ws-SL-tamount-final.

       220-calc-and-move-R-totals.
           move ws-return-count                to ol-R-record.
           move ws-return-tamount              to ol-R-total.
           move ws-R-store-01-tamount          to ol-R-store-01.
           move ws-R-store-02-tamount          to ol-R-store-02.
           move ws-R-store-03-tamount          to ol-R-store-03.
           move ws-R-store-07-tamount          to ol-R-store-07.

           compute ws-R-calc-total = 
               (ws-SL-tamount - ws-return-tamount).

       230-write-SL.
           write C-line                       from ws-SL-headline.
           write C-line                       from ws-newline.
           write C-line                       from ws-S-columns.
           write C-line                       from ws-S-output.
           write C-line                       from ws-newline.
           write C-line                       from ws-L-columns.
           write C-line                       from ws-L-output.
           write C-line                       from ws-newline.
           write C-line                       from ws-SL-total.
           write C-line                       from ws-newline.
           write C-line                       from ws-SL-store-head.
           write C-line                       from ws-SL-store-01.
           write C-line                       from ws-SL-store-02.
           write C-line                       from ws-SL-store-03.
           write C-line                       from ws-SL-store-07.

       240-wrote-R.
           write C-line                        from ws-R-headline.
           write C-line                        from ws-newline.
           write C-line                        from ws-R-columns.
           write C-line                        from ws-R-output.
           write C-line                        from ws-newline.
           write C-line                        from ws-R-store-head.
           write C-line                        from ws-R-store-01.
           write C-line                        from ws-R-store-02.
           write C-line                        from ws-R-store-03.
           write C-line                        from ws-R-store-07.
           write C-line                        from ws-newline.
           write C-line                        from ws-R-grand-total.

       end program DataSplit.