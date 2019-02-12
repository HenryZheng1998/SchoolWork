      *************************************************************
      * The purpose of this program is to produce a detail report *
      * of sales and list the sales and the layaway.               * 
      * ***********************************************************

       identification division.
       program-id. S-LProcessingReport.

       author. Mohammad AL Jarrah Paul Kerrigan.
       date-written. 2018-04-15.


       environment division.
       configuration section.
       input-output section.

       file-control.

      * configure input file

           select input-file
               assign to "../../../../datafiles/data/SL.out"
               organization is line sequential.
                    
      * configure output file
       
           select output-file
               assign to "../../../../datafiles/data/SLProcessing.out"
               organization is line sequential.

       data division.
       file section.

      *declare the input files and record definition

       fd input-file
           data record is input-line
           record contains 36 characters.

       01 input-line.
           05 il-transaction-code                  pic x.
           05 il-transaction-amount                pic 9(5)v99.
           05 il-payment-type                      pic xx.
           05 il-store-number                      pic 99.
           05 il-invoice-number                    pic x(9).
           05 il-sku-code                          pic x(15).

      *declare the output files and record definition 

       fd output-file
           data record is output-line
               record contains 120 characters.

      *declare grad file
       01 output-line                              pic x(120).

       working-storage section.

       01 ws-flags.
           05 ws-eof-flag                          pic x
               value 'N'.

      * create headings 
       01 ws-heading-line1.
           05 filler                               pic x(30)
               value spaces.
           05 filler                               pic x(21)
               value "S&L PROCESSING REPORT".
           05 filler                               pic x(19)
               value spaces.
           05 filler                               pic x(5)
               value "PAGE ". 
           05 ws-ol-page-num                       pic 9
               value 1.
           05 filler                               pic xxx
               value spaces.

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
           05 filler                               pic x(15)
               value spaces.   
           05 filler                               pic x(3)
               value "TAX".


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
           05  filler                              pic x(13)
               value spaces.
           05  filler                              pic x(5)
               value "OWING".


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
           05  filler                              pic x(13)
               value spaces.
           05  filler                              pic x(5)
               value "-----".

      *create details line to output records on report
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
           05 filler                               pic x
               value spaces.
           05 ws-ol-tax-owing                      pic $$$$9.99.

      *calculate total number of S and total amount S records
       01 ws-totals-for-s-redords.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL NUMBER OF 'S' RECORDS: ".
           05 filler                               pic x
               value spaces.
           05 ws-ol-tot-num-s-records              pic 99.
           05 filler                               pic x(10)
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL AMOUNT OF 'S' RECORDS: ".
           05 ws-ol-tot-amount-s-records           pic $$$$,$$9.99.



      *calculate total number of L and total amount L records
       01 ws-totals-for-l-redords.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL NUMBER OF 'L' RECORDS: ".
           05 filler                               pic x
               value spaces.
           05 ws-ol-tot-num-l-records              pic 99.
           05 filler                               pic x(10)
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL AMOUNT OF 'L' RECORDS: ".
           05 ws-ol-tot-amount-l-records           pic $$$$,$$9.99.
      *    05 ws-ol-ws-tot-amount-cash           pic zzzz,zz9.99.


      *calculate the percntag of each payment type 
       01 ws-percntg-of-each-payment-type.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(37)
               value "THE PERCENTAGE OF EACH PAYMENT TYPE: ".
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(12)
               value "CASH 'CA' = ".
           05 ws-ol-tot-percntg-cash               pic 99.99.
           05 ws-ol-percnt-sign1                   pic x.
           05 filler                               pic xxx
               value spaces.
           05 filler                               pic x(19)
               value "CREDIT CARD 'CR' = ".
           05 ws-ol-tot-percntg-credit             pic 99.99.
           05 ws-ol-percnt-sign2                   pic x.
           05 filler                               pic xxx
               value spaces.
           05 filler                               pic x(13)
               value "DEBIT 'DB' = ".
           05 ws-ol-tot-percntg-debit              pic 99.99.
           05 ws-ol-percnt-sign3                   pic x.


      *calculate the total for tax owing
       01 ws-total-tax-owing.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(17)
               value "TOTAL TAX OWING: ".
           05 filler                               pic x
               value spaces.
           05 ws-ol-tot-tax-owing                  pic $$$,$$9.99.
          
      *determine the store number with the highest S&L transaction amont
       01 ws-store-num-highst-trans-amount.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(14)
               value "STORE NUMBER (".
           05 ws-ol-store-num-highest-amount       pic z9
               value 0.
           05 filler                               pic x(34)
               value " ) HAS HIGHEST TRANSACTION AMOUNT.".
           
       
       01 ws-test1.
           05 ws-sub-total                         pic 99
               value 0.
           05 ws-tot-store-amount                  pic 9(6)v99
                                                   occurs 8 times
                                                   value 0.

       

      *set values
       77 ws-sales                                 pic x
           value "S". 
       77 ws-layaway                               pic x
           value "L".

       77 ws-payment-cash                          pic xx
           value "CA".
       77 ws-payment-credit                        pic xx
           value "CR". 
       77 ws-payment-debit                         pic xx
           value "DB".

       77 ws-line-break                            pic x(99)
           value spaces.
       77 ws-page-num                              pic 9
           value 2.
       77 ws-line-count                            pic 99
           value 0. 
       77 ws-lines-per-page                        pic 99
           value 20.

       77 ws-tax-owing                             pic 9(5)v99
           value 0.
       77 ws-tax-value                             pic 9v99
           value 0.13.
       77 ws-tot-num-s-records                     pic 99
           value 0.
       77 ws-tot-amount-s-records                  pic 9(6)v99
           value 0.
       77 ws-tot-num-l-records                     pic 99
           value 0.
       77 ws-tot-amount-l-records                  pic 9(6)v99
           value 0.
       77 ws-overall-tot-amount                    pic 9(5)v99
           value 0.

       01 ws-tot-num-cash                          pic 99
           value 0.
       01 ws-tot-num-credit                        pic 99
           value 0.
       01 ws-tot-num-debit                         pic 99
           value 0.
       01 ws-count                                 pic 999
           value 0.

       01 ws-tot-amount-cash                       pic 999v9999
           value 0.
       01 ws-tot-amount-credit                     pic 999v9999
           value 0.
       01 ws-tot-amount-debit                      pic 999v9999
           value 0.

       01 ws-tot-prcnt-cash                        pic 999v999
           value 0.
       01 ws-tot-prcnt-credit                      pic 999v999
           value 0.
       01 ws-tot-prcnt-debit                       pic 999v999
           value 0.

       77 ws-tot-tax-owing                         pic 9(5)v99
           value 0.

       01 ws-save-amt                              pic 9(6)v99
           value 0.
       01 ws-sub                                   pic 99
           value 0.
       procedure division.
       000-main.

           open input  input-file.
           open output output-file.

      * read initial record from input-file
           read input-file
               at end move "Y"
                   to ws-eof-flag.

           perform 100-print-headings.

           perform 200-process-page
               until ws-eof-flag = "Y".

           write output-line       from ws-totals-for-s-redords
           after advancing 2 lines.

           write output-line       from ws-totals-for-l-redords
           after advancing 2 lines.

           write output-line       from ws-percntg-of-each-payment-type
           after advancing 2 line.

           write output-line       from ws-total-tax-owing
           after advancing 2 line.

           write output-line       from ws-store-num-highst-trans-amount
           after advancing 2 lines.
           
      * close files
           close   input-file.
           close   output-file.
           goback.
       
       100-print-headings.

           write output-line                from ws-heading-line1.
           write output-line                from ws-heading-line2
               after advancing 1 lines.
           write output-line                from ws-heading-line3.
           write output-line                from ws-underlines.
       
       200-process-page.

           move spaces                      to ws-detail-line.
           move ws-page-num                 to ws-ol-page-num.
           move il-transaction-code         to ws-ol-transaction-code.
           move il-transaction-amount       to ws-ol-transaction-amount.
           move il-payment-type             to ws-ol-payment-type.
           move il-store-number             to ws-ol-store-number.
           move il-invoice-number           to ws-ol-invoice-number.
           move il-sku-code                 to ws-ol-sku-code.
           move ws-tot-num-s-records        to ws-ol-tot-num-s-records.
           move ws-tot-num-l-records        to ws-ol-tot-num-l-records.
           move "%"                         to ws-ol-percnt-sign1.
           move "%"                         to ws-ol-percnt-sign2.
           move "%"                         to ws-ol-percnt-sign3.

      *    perform 300-calculations.
           
      *calculate the totals for 'S' and 'L' records
           if ws-line-count >= ws-lines-per-page  then

               write output-line            from ws-heading-line1
                   after advancing 3 lines
               write output-line            from ws-heading-line2
                   after advancing 2 lines
               write output-line            from ws-heading-line3
               write output-line            from ws-underlines
               write output-line            from ws-line-break

               move 0                       to ws-line-count
               add 1                        to ws-page-num
           end-if.


           if il-transaction-code = ws-sales      then
             compute ws-tot-amount-s-records        rounded = 
             ws-tot-amount-s-records + il-transaction-amount
       
             move ws-tot-amount-s-records
             to ws-ol-tot-amount-s-records

               
               add 1                           to ws-tot-num-s-records
               add 1                           to ws-line-count
               add 1                           to ws-count
               perform 400-calc-percentage-of-each-payment-type
               compute ws-tax-owing rounded = 
               il-transaction-amount * ws-tax-value
               move ws-tax-owing               to ws-ol-tax-owing
               write output-line               from ws-detail-line
           else
           
           if il-transaction-code = ws-layaway    then
             compute ws-tot-amount-l-records        rounded = 
             ws-tot-amount-l-records + il-transaction-amount
                                        
             move ws-tot-amount-l-records
             to ws-ol-tot-amount-l-records

               

               add 1                           to ws-line-count
               add 1                           to ws-tot-num-l-records
               add 1                           to ws-count
               perform 400-calc-percentage-of-each-payment-type
               compute ws-tax-owing rounded = 
               il-transaction-amount * ws-tax-value
               move ws-tax-owing               to ws-ol-tax-owing
               write output-line               from ws-detail-line
           end-if
           end-if.

           compute ws-overall-tot-amount          rounded = 
               ws-tot-amount-s-records + ws-tot-amount-l-records.
           
           

          
           read input-file 
               at end move "Y"
                   to ws-eof-flag.


       300-calculations.

      *calculate the tax owing amount 
           compute ws-tax-owing rounded = 
             il-transaction-amount * ws-tax-value.
           move ws-tax-owing                   to ws-ol-tax-owing.

      *calculate the percentage of each payment type
       400-calc-percentage-of-each-payment-type.
           if il-payment-type = ws-payment-cash         then 
               add 1                           to ws-tot-num-cash       
               compute ws-tot-amount-cash rounded =
                   (ws-tot-num-cash / ws-count) 
               compute ws-tot-prcnt-cash  rounded = 
                   ws-tot-amount-cash * 100
               move ws-tot-prcnt-cash        to ws-ol-tot-percntg-cash
               
           else
           if il-payment-type = ws-payment-credit       then 
               add 1 to ws-tot-num-credit
               compute ws-tot-amount-credit rounded = 
                   (ws-tot-num-credit / ws-count) 
               compute ws-tot-prcnt-credit  rounded = 
                   ws-tot-amount-credit * 100
               move ws-tot-prcnt-credit to ws-ol-tot-percntg-credit
           else
           if il-payment-type = ws-payment-debit        then 
               add 1 to ws-tot-num-debit       
               compute ws-tot-amount-debit rounded = 
                    (ws-tot-num-debit / ws-count) 
               compute ws-tot-prcnt-debit  rounded = 
                 ws-tot-amount-debit * 100
               move ws-tot-prcnt-debit to ws-ol-tot-percntg-debit
           end-if
           end-if
           end-if.

           

      *cslculate the total for tax owing
           compute ws-tot-tax-owing                rounded =
             ws-tax-owing + ws-tot-tax-owing.

           move ws-tot-tax-owing      to ws-ol-tot-tax-owing.

      *determine the store number with the highest S&L transaction amont
           move il-store-number       to ws-sub-total.
           add il-transaction-amount          
             to ws-tot-store-amount (ws-sub-total).
           add il-transaction-amount  to ws-tot-store-amount (8).
           
           move 0 to ws-save-amt.
           perform varying ws-sub-total from 1 by 1
             until ws-sub-total = 8

               if ws-tot-store-amount(ws-sub-total) > ws-save-amt then
                   move ws-tot-store-amount(ws-sub-total) to ws-save-amt
                   move ws-sub-total to ws-sub
               end-if
           end-perform.
           move ws-sub to ws-ol-store-num-highest-amount.
       end program S-LProcessingReport.