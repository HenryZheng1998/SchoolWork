      *************************************************************
      * The purpose of this program is to produce a detail report *
      * of sales and list the sales and the layaway.               * 
      * ***********************************************************

       identification division.
       program-id. R-Processing.

       author. Everyone.
       date-written. 2018-04-15.


       environment division.
       configuration section.
       input-output section.

       file-control.

      * configure input file

           select input-file
               assign to "../../../../datafiles/data/returns.out"
               organization is line sequential.
                    
      * configure output file
       
           select output-file
               assign to "../../../../datafiles/data/rprocessing.out"
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
               record contains 99 characters.

      *declare grad file
       01 output-line                              pic x(99).

       working-storage section.

       01 ws-flags.
           05 ws-eof-flag                          pic x
               value 'N'.

      * create headings 

       01 ws-heading-line1.
           05 filler                               pic x(30)
               value spaces.
           05 filler                               pic x(21)
               value "R PROCESSING REPORT".
           05 filler                               pic x(19)
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
           05 fille                                pic x(7)
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
           05 ws-ol-tax-owing                      pic $$$,$$9.99.


      *calculate total number of R and total amount R records
       01 ws-totals-for-r-records.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL NUMBER OF 'R' RECORDS: ".
           05 filler                               pic x
               value spaces.
           05 ws-ol-tot-num-r-records              pic 99.
           05 filler                               pic x(10)
               value spaces.
           05 filler                               pic x(29)
               value "TOTAL AMOUNT OF 'R' RECORDS: ".
           05 ws-ol-tot-amount-r-records           pic $$$$,$$9.99.
      *    05 ws-ol-ws-tot-amount-cash           pic zzzz,zz9.99.


      

      *calculate the total for tax owing
       01 ws-total-tax-owing.
           05 filler                               pic x
               value spaces.
           05 filler                               pic x(17)
               value "TOTAL TAX OWING: ".
           05 filler                               pic x
               value spaces.
           05 ws-ol-tot-tax-owing                  pic $$$9.99.
          
      
           
       
       

      *set values
       77 ws-returns                               pic x
           value "R". 
       77 ws-payment-cash                          pic xx
           value "CA".
       77 ws-payment-credit                        pic xx
           value "CR". 
       77 ws-payment-debit                         pic xx
           value "DB".

       77 ws-line-break                            pic x(99)
           value spaces.
       77 ws-page-num                              pic 9
           value 0.
       77 ws-line-count                            pic 99
           value 0. 
       77 ws-lines-per-page                        pic 99
           value 20.

       77 ws-tax-owing                             pic 9(5)v99
           value 0.
       77 ws-tax-value                             pic 9v99
           value 0.13.
       77 ws-tot-num-r-records                     pic 99
           value 0.
       77 ws-tot-amt-r-records                     pic 9(6)v99
           value 0.
       
       77 ws-tot-tax-owing                         pic 9(5)v99
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

           write output-line       from ws-totals-for-r-records
           after advancing 2 lines.

           write output-line       from ws-total-tax-owing
           after advancing 2 line.

           
           
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
           move il-transaction-code         to ws-ol-transaction-code.
           move il-transaction-amount       to ws-ol-transaction-amount.
           move il-payment-type             to ws-ol-payment-type.
           move il-store-number             to ws-ol-store-number.
           move il-invoice-number           to ws-ol-invoice-number.
           move il-sku-code                 to ws-ol-sku-code.
           move ws-tot-num-r-records        to ws-ol-tot-num-r-records.
           

           

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


           if il-transaction-code = ws-returns      then
           compute ws-tot-amt-r-records        rounded = 
             ws-tot-amt-r-records + il-transaction-amount
       
           move ws-tot-amt-r-records
             to ws-ol-tot-amount-r-records

               
               add 1                           to ws-line-count
               add 1                           to ws-tot-num-r-records
               perform 300-calculations
               write output-line               from ws-detail-line
           end-if.
           read input-file 
               at end move "Y"
                   to ws-eof-flag.


       300-calculations.
           
      *calculate the tax owing amount 
           compute ws-tax-owing rounded = 
             il-transaction-amount * ws-tax-value
           move ws-tax-owing                   to ws-ol-tax-owing.


           perform 400-calc-percentage-of-each-payment-type.


      *calculate the percentage of each payment type
       400-calc-percentage-of-each-payment-type.
          

      *calculate the total for tax owing
      
           add ws-tax-owing to ws-tot-tax-owing.

           move ws-tot-tax-owing      to ws-ol-tot-tax-owing.
           

      

       end program R-Processing.