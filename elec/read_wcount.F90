! compile for big endian:
! ifort read_wcount.F90 -convert big_endian
!
       program read_wcount
       
       implicit none
       
      real, allocatable :: wcount(:,:), wcountout(:,:), wcounttot(:,:), wmaxk(:), wmaxk1(:),altitude(:)
      real, allocatable :: wcountdd(:,:), wcountddout(:,:), wcountddtot(:,:), wmink(:), wmink1(:),altitudedd(:)
      real, allocatable :: udmf(:),ddmf(:),udmf3(:),ddmf3(:),mpitot(:)
      integer :: numwcount = 500
      integer :: kme,numarg
      character(len=100)          :: arg
      integer                     :: i,iout,j,k
      real tmpg, tmph, tmppwf, tmps, tmpi, tot, tottmp, tot2ms, tottmp2ms
      integer i1,i50,i90,i99,i90a,i99a

      
      numarg = command_argument_count()
      write(0,*) 'numarg = ',numarg
      do i = 1, numarg
       call get_command_argument(1, arg)
       iout = 15
       open(unit=iout,status='old',file=arg,form='unformatted')
       read(iout) kme,numwcount
       write(0,*) 'kme,numwcount = ',kme,numwcount
      
       IF ( i == 1 ) THEN
       allocate( wcount(numwcount,kme) )
       allocate( wcounttot(numwcount,kme) )
       allocate( wmaxk(kme), wmaxk1(kme) )
       allocate( udmf(kme),ddmf(kme),udmf3(kme),ddmf3(kme),mpitot(kme) )
       allocate( altitude(kme) )
       allocate( wcountout(numwcount,kme) )
       allocate( wcountdd(numwcount,kme) )
       allocate( wcountddtot(numwcount,kme) )
       allocate( wmink(kme), wmink1(kme) )
       allocate( altitudedd(kme) )
       allocate( wcountddout(numwcount,kme) )

      read(iout) altitude,wmaxk,udmf,udmf3,wmink,ddmf,ddmf3
      read(iout) wcount,wcountdd
      read(iout) wcounttot,wcountddtot
       
       ELSE ! extra files; need to add to arrays
       
      read(iout) altitude,wmaxk1,udmf,udmf3,wmink1,ddmf,ddmf3
      read(iout) wcount,wcountdd
      read(iout) wcountout,wcountddout ! wcounttot,wcountddtot
       
          DO k = 1,kme-1
            wmaxk(k) = Max( wmaxk(k), wmaxk1(k) )
            wmink(k) = Min( wmink(k), wmink1(k) )
            DO j = 1,numwcount
              wcounttot(j,k) = wcounttot(j,k) + wcountout(j,k)
              wcountddtot(j,k) = wcountddtot(j,k) + wcountddout(j,k)
            ENDDO
          ENDDO

       
       ENDIF

      
      close(iout)
      
      ENDDO
      

      write(6,*) 'k,altitude(km),w50,w90,w99,w90 (w>2), w99 (w>2),wmax,numsample,numsample2,udmf,udmf3'
      DO k = kme-1,1,-1
        tot = 0.0
        tot2ms = 0.0
        tottmp = 0.0
        tottmp2ms = 0.0
        DO i = 10,numwcount
          tot = tot + wcounttot(i,k)
          IF ( i > 20 ) tot2ms = tot2ms + wcounttot(i,k) ! total greater than 2m/s
        ENDDO

         i50 = 0
         i90 = 0
         i99 = 0
         i90a = 0
         i99a = 0

        IF ( tot > 0.0 ) THEN
          DO i = 10,numwcount
            tottmp = tottmp + wcounttot(i,k)
            IF ( tottmp <= 0.50*tot ) THEN
              i50 = i
            ENDIF
            IF ( tottmp <= 0.90*tot ) THEN
              i90 = i
            ENDIF
            IF ( tottmp <= 0.99*tot ) THEN
              i99 = i
            ENDIF
            
            IF ( i > 20 .and. tot2ms > 1.0 ) THEN
              tottmp2ms = tottmp2ms + wcounttot(i,k)
              IF ( tottmp2ms <= 0.90*tot2ms ) THEN
                i90a = i
              ENDIF
              IF ( tottmp2ms <= 0.99*tot2ms ) THEN
                i99a = i
              ENDIF
            ENDIF

          ENDDO
        ENDIF
       
!        write(6,*) k,0.1*i50,0.1*i90,0.1*i99,wmaxk(k),tot,tottmp
        write(6,'(1x,i3,1x,f7.3,6(1x,f7.2),4(2x,1pe13.5))') &
           k,altitude(k),0.1*i50,0.1*i90,0.1*i99, 0.1*i90a, 0.1*i99a, wmaxk(k),tot,tot2ms, udmf(k),udmf3(k)
        
        
      ENDDO
      
      
      END
