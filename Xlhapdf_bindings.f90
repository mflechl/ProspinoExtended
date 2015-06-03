module xx_lhapdf_bindings
!  use xx_pass_integ
  use xx_public_variables
   character(len=64), private  :: lha_set_lo, lha_set_nlo
   integer,           private  :: lha_num_lo, lha_num_nlo
   logical,           private  :: cur_lha_is_nlo, use_lhaals_lo, use_lhaals_nlo
   logical,           private  :: nlo_eq_lo
   double precision,  private  :: lambda_qcd_lo, lambda_qcd_nlo
   double precision,  private  :: xmin_lo, xmax_lo, xmin_nlo, xmax_nlo
   double precision,  private  :: qmin_lo, qmax_lo, qmin_nlo, qmax_nlo 
   double precision,  private  :: alphas_lo(1:10000), alphas_nlo(1:10000)
   public  :: INIT_LHAPDF, INIT_PDF, ALPHAS
!   private :: INIT_LHA_ALPHAS
contains

!   Initialize a pdf-set from lha.
!   Uses file named set, and replica num from set.
!   With use_lhaals_lo/nlo you can determine weather to use the alphas_s
!   from pdf or from internal routine.
  subroutine INIT_LHAPDF(set_lo, num_lo, iuse_lhaals_lo, set_nlo, num_nlo, iuse_lhaals_nlo)

    implicit none
    character(len=64),   intent(in) :: set_lo, set_nlo
    integer,             intent(in) :: num_lo, num_nlo
    logical,             intent(in) :: iuse_lhaals_lo, iuse_lhaals_nlo
    double precision                :: als, als_temp, lambda_temp, ALPHAS_OLD, M_Z
    integer                         :: l_digit
    double precision                :: acc, mcq, mbq, mtq
    integer                         :: nq

    integer max_num

    logical ldebug
    parameter ( ldebug=.false.)

    M_Z = 91.11876e0 

    ! Make lhapdf be quiet
    if (.not.ldebug) call SetLHAPARM('SILENT')

    lha_set_lo = set_lo
    lha_num_lo = num_lo
    lha_set_nlo = set_nlo
    lha_num_nlo = num_nlo
    use_lhaals_lo = iuse_lhaals_lo
    use_lhaals_nlo = iuse_lhaals_nlo

    ! Are the two sets the same? Then there is no need for reinit later.
    if ( (lha_set_lo.eq.lha_set_nlo) .and. (lha_num_lo.eq.lha_num_nlo) ) then
       nlo_eq_lo = .true. 
    else
       nlo_eq_lo = .false.
    end if

    cur_lha_is_nlo = .true.

    call INIT_PDF(0)

    call numberPDF(max_num)

    if (num_lo.gt.max_num) then
       print*, "There are only ",max_num," replicas in the pdf-set, you tried to get number ",num_lo
       print*, "Set is: ", set_lo
       call HARD_STOP
    end if

    call GetLam5(num_lo, lambda_qcd_lo)

    if (lambda_qcd_lo.eq.0.0d0) then
       print*, "# PDF contains no lambda_qcd value for lo. Trying to calculate the correct lambda_qcd for LO."
       
       use_lhaals_lo = .true. 
       als = ALPHAS(M_Z, 1)
       use_lhaals_lo = iuse_lhaals_lo
!       print*, "     as(MZ) = ", als

       lambda_temp = 0.0

       do l_digit = 1,6
            do
                  lambda_temp = lambda_temp + 10e0**(-1*l_digit)

                  acc = 1.e-8
                  mcq = 1.5e0
                  mbq = 4.75e0 
                  mtq = 175000.0e0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

                  als_temp = ALPHAS_OLD(M_Z, 1)
!                  print*, "        lambda = ",lambda_temp,"   as(MZ) = ",als_temp
                  
                  if (als_temp.gt.als) exit
            end do
            
            lambda_temp = lambda_temp - 10e0**(-1*l_digit)

                  acc = 1.e-8
                  mcq = 1.5
                  mbq = mb
                  mtq = 175000.0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

            als_temp = ALPHAS_OLD(M_Z, 1)
!            print*, "       rewind:  lambda = ",lambda_temp,"   as(MZ) = ",als_temp
       end do

       !lambda_qcd_lo = 0.32215d0
       lambda_qcd_lo = lambda_temp 

                  acc = 1.e-8
                  mcq = 1.5
                  mbq = mb
                  mtq = 175000.0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

       als_temp = ALPHAS_OLD(M_Z, 1)
       print*, "    set lambda_qcd to ", lambda_qcd_lo, " to get a_s(M_Z) = ", als_temp, " in LO"

!       print*, "# PDF contains no lambda_qcd value for lo. Setting to default 0.2551."
!       lambda_qcd_lo = 0.2551d0
    end if

    ! Fill min and max values for lo-pdf.
    call GetXmin(num_lo, xmin_lo)
    call GetXmax(num_lo, xmax_lo)
    call GetQ2min(num_lo, qmin_lo)
    qmin_lo = sqrt(qmin_lo)
    call GetQ2max(num_lo, qmax_lo)
    qmax_lo = sqrt(qmax_lo)

!    call INIT_LHA_ALPHAS(.false.)

    !print*, "lo:"
    !print*, xmin_lo, " ", xmax_lo

    call INIT_PDF(1)

    call numberPDF(max_num)

    if (num_nlo.gt.max_num) then
       print*, "There are only ",max_num," replicas in the pdf-set, you tried to get number ",num_nlo
       print*, "Set is: ", set_nlo
       call HARD_STOP
    end if


    call GetLam5(num_nlo, lambda_qcd_nlo)

    if (lambda_qcd_nlo.eq.0.0d0) then
       print*, "# PDF contains no lambda_qcd value for nlo. Trying to calculate the correct lambda_qcd for NLO."
       
       use_lhaals_nlo = .true. 
       als = ALPHAS(M_Z, 2)
       use_lhaals_nlo = iuse_lhaals_nlo
!       print*, "     as(MZ) = ", als

       lambda_temp = 0.0

       do l_digit = 1,6
            do
                  lambda_temp = lambda_temp + 10e0**(-1*l_digit)

                  acc = 1.e-8
                  mcq = 1.5e0
                  mbq = 4.75e0 
                  mtq = 175000.0e0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

                  als_temp = ALPHAS_OLD(M_Z, 2)
!                  print*, "        lambda = ",lambda_temp,"   as(MZ) = ",als_temp
                  
                  if (als_temp.gt.als) exit
            end do
            
            lambda_temp = lambda_temp - 10e0**(-1*l_digit)

                  acc = 1.e-8
                  mcq = 1.5
                  mbq = mb
                  mtq = 175000.0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

            als_temp = ALPHAS_OLD(M_Z, 2)
!            print*, "       rewind:  lambda = ",lambda_temp,"   as(MZ) = ",als_temp
       end do

       !lambda_qcd_lo = 0.32215d0
       lambda_qcd_nlo = lambda_temp 

                  acc = 1.e-8
                  mcq = 1.5
                  mbq = mb
                  mtq = 175000.0

                  nq = 5

                  call ALSINI(acc, lambda_temp, mcq, mbq, mtq, nq)

       als_temp = ALPHAS_OLD(M_Z, 2)
       print*, "    set lambda_qcd to ", lambda_qcd_nlo, " to get a_s(M_Z) = ", als_temp, " in NLO"


!       print*, "# PDF contains no lambda_qcd value for nlo. Setting to default 0.2551."
!       lambda_qcd_nlo = 0.2551d0
    end if

    ! Fill min and max values for nlo-pdf.
    call GetXmin(num_nlo, xmin_nlo)
    call GetXmax(num_nlo, xmax_nlo)
    call GetQ2min(num_nlo, qmin_nlo)
    qmin_nlo = sqrt(qmin_nlo)
    call GetQ2max(num_nlo, qmax_nlo)
    qmax_nlo = sqrt(qmax_nlo)

!    call INIT_LHA_ALPHAS(.true.)

    print*, "Using for lo:"
    print*, "   ", set_lo 
    print*, "   set ", num_lo 
    print*, "   lambda_qcd = ", lambda_qcd_lo
    print*, "   using alphas from pdf: ", use_lhaals_lo
    print*, "and for nlo:"
    print*, "   ", set_nlo
    print*, "   set ", num_nlo  
    print*, "   lambda_qcd = ", lambda_qcd_nlo
    print*, "   using alphas from pdf: ", use_lhaals_nlo
    print*, ""

!    print*, "nlo:"
!    print*, xmin_nlo, " ", xmax_nlo

  end subroutine


! Initialize the lo or nlo set previously defined with INIT_LHAPDF
  subroutine INIT_PDF(inlo)
  
     implicit none
     integer, intent(in)     :: inlo
     logical                 :: nlo

     if(inlo.le.0) then
         nlo = .false.
     else 
         nlo = .true.
     end if

     ! Could be, that another order is required
     if (nlo.neqv.cur_lha_is_nlo) then
        if (.not.nlo) then
!           print*, lha_set_lo
           call InitPDFsetByName(lha_set_lo)
           call InitPDF(lha_num_lo)
        else 
!           print*, lha_set_nlo
           call InitPDFsetByName(lha_set_nlo)
           call InitPDF(lha_num_nlo)
        end if
     end if

     ! Maybe we don't need to change the pdf on next call.
     cur_lha_is_nlo = nlo

  end subroutine

! Get Lambda qcd for lo or nlo order. 
  subroutine GET_LAMBDA_QCD(nlo, lambda_qcd)

     implicit none
     logical,          intent(in)  :: nlo
     double precision, intent(out) :: lambda_qcd

!     call GET_LAMBDA_QCD_OLD(nlo, lambda_qcd)
!     return

     if (nlo) then
        lambda_qcd = lambda_qcd_nlo
     else
        lambda_qcd = lambda_qcd_lo 
     end if

  end subroutine

! Get a pdf(-6:6) array with according to lhapdf-particle-numbers.
  subroutine GET_PDF(inlo, x, mu, pdf)

     implicit none
     integer, intent(in) :: inlo
     integer i1
     double precision, intent(in) :: x, mu
     double precision, intent(out) :: pdf(-6:6)
     double precision xmin, xmax, qmin, qmax, swap

     logical ldebug
     parameter ( ldebug=.false.)

!     call GET_PDF_OLD(inlo, x, mu, pdf)
!
!     return

     if (ldebug) print*, "GET_PDF(",inlo,", ", x, ", ", mu, ", pdf) called."

     do i1=-6,6,1
        pdf(i1) = 0.0d0
     end do

     if (inlo.le.0) then
        if ( (x.lt.xmin_lo) .or. (x.gt.xmax_lo) ) then
           print*, xmin_lo, " ", xmax_lo
           print*, "GET_PDF: x = ", x, " exceeds bounds for pdf."
           return
        end if
        if ( (mu.lt.qmin_lo) .or. (mu.gt.qmax_lo) ) then
           print*, xmin_nlo, " ", xmax_nlo
           print*, "GET_PDF: mu = ", x, " exceeds bounds for pdf."
           return
        end if
     else 
        if ( (x.lt.xmin_nlo) .or. (x.gt.xmax_nlo) ) then
           print*, "GET_PDF: x = ", x, " exceeds bounds for pdf."
           return
        end if
        if ( (mu.lt.qmin_nlo) .or. (mu.gt.qmax_nlo) ) then
           print*, "GET_PDF: mu = ", x, " exceeds bounds for pdf."
           return
        end if
     end if     

     call INIT_PDF(inlo)
     
     call evolvePDF(x, mu, pdf)

     do i1=-6,6,1
        pdf(i1) = pdf(i1)/x
     end do

     ! In LHAPDF the i in pdf(i) means d u s c b t
     ! the internal definition of prospino is u d s c b t
     swap = pdf(1)
     pdf(1) = pdf(2)
     pdf(2) = swap

     swap = pdf(-1)
     pdf(-1) = pdf(-2)
     pdf(-2) = swap

  end subroutine

! I don't like that, but at the moment i see no other chance: It seems
! like i have to grab alphas values from lo and nlo-pdf and store them
! to interpolate between them later. 
! If i would call alphasPDF from LHA, it would be necessary to reinitialize
! the lo and nlo pdfs over and over again, which takes a lot of time.
!
! Theres one extra piece of problem, that is, that maybe the lo pdf doesn't
! contain an lo-alphas evolution. I will use the original ALPHAS from prospino
! for that cases, initialized with alphas-masses and lambda-qcd from lo-pdf.
! The last thing is, that there might be no lambda_qcd defined (like 
! lambda_qcd = 0 in MSTW2008). For now i will complain and stop on that problem.

!  subroutine INIT_LHA_ALPHAS(nlo)
!
!     implicit none
!
!     logical nlo
!     integer scal, order
!     double precision als, alphasPDF, ALPHAS_OLD
!     double precision lambda_qcd, amc, amb, amt
!
!     if (.not.nlo) then
!        call INIT_PDF(0)
!
!        call GetOrderAs(order)
!
!        if ((order.eq.0).and.use_lhaals_lo )then
!           do scal = 1, 10000, 1
!              if ((scal * 0.1d0).ge.qmin_lo) then
!                 als = alphasPDF(scal * 0.1d0)
!              else
!                 als = 0.0d0
!              end if
!              alphas_lo(scal) = als
!           end do
!        else 
!           ! Init original alphas
!           call GET_LAMBDA_QCD(.false., lambda_qcd)
!           call GetQmass(4, amc)
!           call GetQmass(5, amb)
!           call GetQmass(6, amt)
!
!           print*, "# Used alphas-fallback for lo."
!           print*, "# lambda_qcd = ", lambda_qcd
!           print*, "# m_charme   = ", amc 
!           print*, "# m_bottom   = ", amb 
!           print*, "# m_top      = ", amt 
!
!           call ALSINI(1.0d-8, lambda_qcd, amc, amb, amt, 5)
!
!           do scal = 1, 10000, 1
!              als = ALPHAS_OLD(scal * 0.1d0, 1)
!              alphas_lo(scal) = als
!           end do
!
!        end if 
!     else
!        call INIT_PDF(1)
!
!        call GetOrderAs(order)
!
!        if ((order.eq.1).and.use_lhaals_nlo) then
!           do scal = 1, 10000, 1
!              if ((scal * 0.1d0).ge.qmin_nlo) then
!                 als = alphasPDF(scal * 0.1d0)
!              else
!                 als = 0.0d0
!              end if
!              alphas_nlo(scal) = als
!           end do
!        else 
!           ! Init original alphas
!           call GET_LAMBDA_QCD(.true., lambda_qcd)
!           call GetQmass(4, amc)
!           call GetQmass(5, amb)
!           call GetQmass(6, amt)
!
!           print*, "# Used alphas-fallback for nlo."
!           print*, "# lambda_qcd = ", lambda_qcd
!           print*, "# m_charme   = ", amc 
!           print*, "# m_bottom   = ", amb 
!           print*, "# m_top      = ", amt 
!
!           call ALSINI(1.0d-8, lambda_qcd, amc, amb, amt, 5)
!
!           do scal = 1, 10000, 1
!              als = ALPHAS_OLD(scal * 0.1d0, 2)
!              alphas_nlo(scal) = als
!           end do
!        end if 
!      end if
!  end 


  double precision function ALPHAS(q,n)

     implicit none

     integer n
     double precision q, ALPHAS_OLD, alphasPDF


     if (((n.eq.1).and.(.not.use_lhaals_lo)).or.((n.eq.2).and.(.not.use_lhaals_nlo))) then
         ALPHAS = ALPHAS_OLD(q,n)
         return
     end if

     if (n.eq.1) then
         call INIT_PDF(0)
     else
         call INIT_PDF(1)
     end if

     ALPHAS = alphasPDF(q)
     return
  end function
!     integer n, nlo, li, ri
!     double precision q, lval, rval, a, b, ALPHAS_OLD
!
!! just use the old one
!
!!     ALPHAS = ALPHAS_OLD(q, n)
!!     return
!
!     ! Uses other ids for lo and nlo!
!     if(n.eq.1) then 
!        nlo = 0
!     else
!        nlo = 1
!     end if
!
!     if (q.le.10.0d0) then
!        print*, "Q for alphas must be greater than 10.0."
!        call HARD_STOP
!     end if
!
!     if (q.ge.1000.0d0) then
!        print*, "Q for alphas must be less than 1000.0."
!        call HARD_STOP
!     end if
!
!     li = FLOOR(q*10.0)
!     ri = FLOOR(q*10.0+1.0d0)
!
!     if (nlo.eq.1) then
!        lval = alphas_nlo(li)
!        rval = alphas_nlo(ri)
!     else
!        lval = alphas_lo(li)
!        rval = alphas_lo(ri)
!     end if 
!
!!  Linear interpolation
!!     ALPHAS = lval + (rval - lval)*(q - 1.0d0*li)
!
!! log interpolation
!     a = (rval-lval)/(log(0.1d0*ri) - log(0.1d0*li))
!     b = lval - a * log(0.1d0*li)
!
!     ALPHAS = a * log(q) + b
!
!
!!     call INIT_PDF(nlo)
!
!!     ALPHAS = alphasPDF(q)
!  end




!  subroutine TEST_ALPHAS(nlo)
!
!     implicit none
!
!     logical nlo
!     double precision                     :: alphasPDF, als1, als2, als3, ALPHAS_OLD
!     integer                              :: scal
!!
!     print*, "# scale          interpolated         from pdf           internal        dif(interpolated, from pdf)"
!
!     if (.not.nlo) then
!        call INIT_PDF(0)
!        call ALSINI(1.0d-8,lambda_qcd_lo, 1.5d0, 4.6d0, 175000.0, 5)
!        do scal = 10, 999, 1
!           als1 = ALPHAS(scal*1.0D0+0.5d0, 1)
!           als2 = alphasPDF(scal*1.0d0 + 0.5d0)
!           als3 = ALPHAS_OLD(scal*1.0d0 + 0.5d0, 1)
!           print*,scal*1.0d0+0.5d0," ",als1," ", als2," ", als3, " " ,(als2-als1)
!        end do
!     else
!        call INIT_PDF(1)
!        call ALSINI(1.0d-8,lambda_qcd_nlo, 1.5d0, 4.6d0, 175000.0, 5)
!        do scal = 10, 999, 1
!           als1 = ALPHAS(scal*1.0D0+0.5d0, 2)
!           als2 = alphasPDF(scal*1.0d0 + 0.5d0)
!           als3 = ALPHAS_OLD(scal*1.0d0 + 0.5d0, 2)
!           print*,scal*1.0d0+0.5d0," ",als1," ", als2," ", als3, " ", (als2-als1)
!        end do
!     end if
!
!  end

  
  subroutine TEST_PDF()

     implicit none

     character(len=64)  lo_set, nlo_set
     double precision   pdf1(-6:6), pdf2(-6:6)
     integer i 

     lo_set = "cteq6ll.LHpdf"
     nlo_set = "cteq66.LHgrid"

     call INIT_LHAPDF(lo_set, 0, .false., nlo_set, 0, .false.)

     print*, "NEXT TO LEADING_ORDER at mu = 69GeV"
     print*, ""

     call INIT_PDF_OLD(1) 

     print*, "x     pdf_b_orig     pdf_b_lhapdf      pdf_g_orig       pdf_b_lhapdf"

     do i = 1,99,1
        call GET_PDF_OLD(1, i*0.01d0, 69.0d0, pdf1)
        call GET_PDF(1, i*0.01d0, 69.0d0, pdf2)
        print*, i*0.01d0," ",pdf1(5)," ",pdf2(5)," ",pdf1(0)," ",pdf2(0)
     end do

     print*, ""
     print*, ""
     print*, "LEADING_ORDER at mu = 69GeV"
     print*, ""

     call INIT_PDF_OLD(0) 

     print*, "x     pdf_b_orig     pdf_b_lhapdf      pdf_g_orig       pdf_b_lhapdf"

     do i = 1,100,1
        call GET_PDF_OLD(0, i*0.01d0, 69.0d0, pdf1)
        call GET_PDF(0, i*0.01d0, 69.0d0, pdf2)
        print*, i*0.01d0," ",pdf1(5)," ",pdf2(5)," ",pdf1(0)," ",pdf2(0)
     end do
  end subroutine

end module xx_lhapdf_bindings
