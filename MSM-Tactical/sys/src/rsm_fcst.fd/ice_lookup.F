!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!--------------- creates lookup tables for ice processes ---------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      subroutine ice_lookup
!
!-----------------------------------------------------------------------------------
!
!---- key diameter values in mm
!
!-----------------------------------------------------------------------------------
!
!---- key concepts:
!       - actual physical diameter of particles (d)
!       - ratio of actual particle diameters to mean diameter (x=d/md)
!       - mean diameter of exponentially distributed particles, which is the
!         same as 1./lamda of the distribution (md)
!       - all quantitative relationships relating ice particle characteristics as
!         functions of their diameter (e.g., ventilation coefficients, normalized
!         accretion rates, ice content, and mass-weighted fall speeds) are a result
!         of using composite relationships for ice crystals smaller than 1.5 mm
!         diameter merged with analogous relationships for larger sized aggregates.
!         relationships are derived as functions of mean ice particle sizes assuming
!         exponential size spectra and assuming the properties of ice crystals at
!         sizes smaller than 1.5 mm and aggregates at larger sizes.  
!
!-----------------------------------------------------------------------------------
!
!---- actual ice particle diameters for which integrated distributions are derived
!       - dmini - minimum diameter for integration (.02 mm, 20 microns)
!       - dmaxi - maximum diameter for integration (2 cm)
!       - ddeli - interval for integration (1 micron)
!
      real, parameter :: dmini=.02e-3, dmaxi=20.e-3, ddeli=1.e-6,        &   
     &  ximin=1.e6*dmini, ximax=1.e6*dmaxi
      integer, parameter :: idimin=ximin, idimax=ximax
!
!---- meaning of the following arrays:
!        - diam - ice particle diameter (m)
!        - mass - ice particle mass (kg)
!        - vel  - ice particle fall speeds (m/s)
!        - vent1 - 1st term in ice particle ventilation factor
!        - vent2 - 2nd term in ice particle ventilation factor
!
      real diam(idimin:idimax),mass(idimin:idimax),vel(idimin:idimax),   &   
     & vent1(idimin:idimax),vent2(idimin:idimax)
!
!-----------------------------------------------------------------------------------
!
!---- found from trial & error that the m(d) & v(d) mass & velocity relationships
!       between the ice crystals and aggregates overlapped & merged near a particle
!       diameter sizes of 1.5 mm.  thus, ice crystal relationships are used for
!       sizes smaller than 1.5 mm and aggregate relationships for larger sizes.
!
      real, parameter :: d_crystal_max=1.5
!
!---- the quantity xmax represents the maximum value of "x" in which the
!       integrated values are calculated.  for xmax=20., this means that
!       integrated ventilation, accretion, mass, and precipitation rates are
!       calculated for ice particle sizes less than 20.*mdiam, the mean particle diameter.
!
      real, parameter :: xmax=20.
!
!-----------------------------------------------------------------------------------
!
!-----------------------------------------------------------------------------------
!
!--- parameters for ice lookup tables, which establish the range of mean ice particle
!      diameters; from a minimum mean diameter of 0.05 mm (dmimin) to a 
!      maximum mean diameter of 1.00 mm (dmimax).  the tables store solutions
!      at 1 micron intervals (deldmi) of mean ice particle diameter.  
!
      real, parameter :: dmimin=.05e-3, dmimax=1.e-3, deldmi=1.e-6,      &      
     &  xmimin=1.e6*dmimin, xmimax=1.e6*dmimax
      integer, parameter :: mdimin=xmimin, mdimax=xmimax
!
!---- meaning of the following arrays:
!        - mdiam - mean diameter (m)
!        - venti1 - integrated quantity associated w/ ventilation effects
!                   (capacitance only) for calculating vapor deposition onto ice
!        - venti2 - integrated quantity associated w/ ventilation effects
!                   (with fall speed) for calculating vapor deposition onto ice
!        - accri  - integrated quantity associated w/ cloud water collection by ice
!        - massi  - integrated quantity associated w/ ice mass 
!        - vsnowi - mass-weighted fall speed of snow, used to calculate precip rates
!
!--- mean ice-particle diameters varying from 50 microns to 1000 microns (1 mm), 
!      assuming an exponential size distribution.  
!
      real mdiam
      common /ivent_tables/ venti1(mdimin:mdimax), venti2(mdimin:mdimax)
      common /iaccr_tables/ accri(mdimin:mdimax)
      common /imass_tables/ massi(mdimin:mdimax)
      real massi
      common /irate_tables/ vsnowi(mdimin:mdimax)
!
!-----------------------------------------------------------------------------------
!------------- constants & parameters for ventilation factors of ice ---------------
!-----------------------------------------------------------------------------------
!
!---- these parameters are used for calculating the ventilation factors for ice
!       crystals between 0.2 and 1.5 mm diameter (hall and pruppacher, jas, 1976).  
!       from trial & error calculations, it was determined that the ventilation
!       factors of smaller ice crystals could be approximated by a simple linear
!       increase in the ventilation coefficient from 1.0 at 50 microns (.05 mm) to 
!       1.1 at 200 microns (0.2 mm), rather than using the more complex function of
!       1.0 + .14*(sc**.33*re**.5)**2 recommended by hall & pruppacher.
!
      real, parameter :: cvent1i=.86, cvent2i=.28
!
!---- these parameters are used for calculating the ventilation factors for larger
!       aggregates, where d>=1.5 mm (see rutledge and hobbs, jas, 1983; 
!       thorpe and mason, 1966).
!
      real, parameter :: cvent1a=.65, cvent2a=.44
!
      real m_agg,m_bullet,m_column,m_ice,m_plate
!
!---- various constants
!
      real, parameter :: c1=2./3., cexp=1./3.
!
      logical :: iprint
      logical, parameter :: print_diag=.false.
!
!-----------------------------------------------------------------------------------
!- constants & parameters for calculating the increase in fall speed of rimed ice --
!-----------------------------------------------------------------------------------
!
!---- constants & arrays for estimating increasing fall speeds of rimed ice.
!     based largely on theory and results from bohm (jas, 1989, 2419-2427).
!
!-------------------- standard atmosphere conditions at 1000 mb --------------------
!
      real, parameter :: t_std=288., dens_std=1000.e2/(287.04*288.)
!
!---- these "bulk densities" are the actual ice densities in the ice portion of the 
!     lattice.  they are based on text associated w/ (12) on p. 2425 of bohm (jas, 
!     1989).  columns, plates, & bullets are assumed to have an average bulk density 
!     of 850 kg/m**3.  aggregates were assumed to have a slightly larger bulk density 
!     of 600 kg/m**3 compared with dendrites (i.e., the least dense, most "lacy" & 
!     tenous ice crystal, which was assumed to be ~500 kg/m**3 in bohm).  
!
      real, parameter :: dens_crystal=850., dens_agg=600.
!
!--- a value of nrime=40 for a logarithmic ratio of 1.1 yields a maximum rime factor
!      of 1.1**40 = 45.26 that is resolved in these tables.  this allows the largest
!      ice particles with a mean diameter of mdimax=1000 microns to achieve bulk 
!      densities of 900 kg/m**3 for rimed ice.  
!
      integer, parameter :: nrime=40
      real m_rime, rime_factor(0:nrime), rime_vel(0:nrime),              &  
     &     vel_rime(idimin:idimax,nrime), ivel_rime(mdimin:mdimax,nrime)
      common /irime_tables/ vel_rf(2:9,0:nrime)
!
!
!-----------------------------------------------------------------------------------
!----------------------------- begin execution -------------------------------------
!-----------------------------------------------------------------------------------
!
!
      c2=1./sqrt(3.)
      pi=acos(-1.)
      cbulk=6./pi
      cbulk_ice=900.*pi/6.    ! maximum bulk ice density allowed of 900 kg/m**3
      px=.4**cexp             ! convert fall speeds from 400 mb (starr & cox) to 1000 mb
!
!--------------------- dynamic viscosity (1000 mb, 288 k) --------------------------
!
      dynvis_std=1.496e-6*t_std**1.5/(t_std+120.)
      crime1=pi/24.
      crime2=8.*9.81*dens_std/(pi*dynvis_std**2)
      crime3=crime1*dens_crystal
      crime4=crime1*dens_agg
      crime5=dynvis_std/dens_std
      do i=0,nrime
        rime_factor(i)=1.1**i
      enddo
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!      characteristics as functions of actual ice particle diameter 
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
!----   m(d) & v(d) for 3 categories of ice crystals described by starr 
!----   & cox (1985). 
!
!----   capacitance & characteristic lengths for reynolds number calculations
!----   are based on young (1993; p. 144 & p. 150).  c-axis & a-axis 
!----   relationships are from heymsfield (jas, 1972; table 1, p. 1351).
!
      icount=60
!
      if (print_diag)                                                    &  
     &  write(7,"(2a)") '---- increase in fall speeds of rimed ice',     &
     &    ' particles as function of ice particle diameter ----'
      do i=idimin,idimax
        if (icount.eq.60 .and. print_diag) then
          write(6,"(/2a/3a)") 'particle masses (mg), fall speeds ',      &
     &      '(m/s), and ventilation factors',                            &
     &      '  d(mm)  cr_mass   mass_bull   mass_col  mass_plat ',       &
     &      '  mass_agg   cr_vel  v_bul cr_col cr_pla aggreg',           &
     &      '    vent1      vent2 '
          write(7,"(3a)") '        <----------------------------------', &
     &      '---------------  rime factor  --------------------------',  &
     &      '--------------------------->'
          write(7,"(a,23f5.2)") '  d(mm)',(rime_factor(k), k=1,5),       &
     &       (rime_factor(k), k=6,40,2)
          icount=0
        endif
        d=(float(i)+.5)*1.e-3    ! in mm
        c_avg=0.
        c_agg=0.
        c_bullet=0.
        c_column=0.
        c_plate=0.
        cl_agg=0.
        cl_bullet=0.
        cl_column=0.
        cl_plate=0.
        m_agg=0.
        m_bullet=0.
        m_column=0.
        m_plate=0.
        v_agg=0.
        v_bullet=0.
        v_column=0.
        v_plate=0.
        if (d .lt. d_crystal_max) then
!
!---- this block of code calculates bulk characteristics based on average
!     characteristics of bullets, plates, & column ice crystals <1.5 mm size
!
!---- mass-diameter relationships from heymsfield (1972) & used
!       in starr & cox (1985), units in mg
!---- "d" is maximum dimension size of crystal in mm, 
!
! mass of pure ice for spherical particles, used as an upper limit for the
!   mass of small columns (<~ 80 microns) & plates (<~ 35 microns)
!
          m_ice=.48*d**3   ! mass of pure ice for spherical particle
!
          m_bullet=min(.044*d**3, m_ice)
          m_column=min(.017*d**1.7, m_ice)
          m_plate=min(.026*d**2.5, m_ice)
!
          mass(i)=m_bullet+m_column+m_plate
!
!---- these relationships are from starr & cox (1985), applicable at 400 mb
!---- "d" is maximum dimension size of crystal in mm, dx in microns
!
          dx=1000.*d            ! convert from mm to microns
          if (dx .le. 200.) then
            v_column=8.114e-5*dx**1.585
            v_bullet=5.666e-5*dx**1.663
            v_plate=1.e-3*dx
          else if (dx .le. 400.) then
            v_column=4.995e-3*dx**.807
            v_bullet=3.197e-3*dx**.902
            v_plate=1.48e-3*dx**.926
          else if (dx .le. 600.) then
            v_column=2.223e-2*dx**.558
            v_bullet=2.977e-2*dx**.529
            v_plate=9.5e-4*dx
          else if (dx .le. 800.) then
            v_column=4.352e-2*dx**.453
            v_bullet=2.144e-2*dx**.581
            v_plate=3.161e-3*dx**.812
          else 
            v_column=3.833e-2*dx**.472
            v_bullet=3.948e-2*dx**.489
            v_plate=7.109e-3*dx**.691
          endif
!
!---- reduce fall speeds from 400 mb to 1000 mb
!
          v_column=px*v_column
          v_bullet=px*v_bullet
          v_plate=px*v_plate
!
!---- different version!  calculates mass-weighted crystal fall speeds
!
          vel(i)=(m_bullet*v_bullet+m_column*v_column+m_plate*v_plate)/  &   
     &           mass(i)
          mass(i)=mass(i)/3.
!
!---- shape factor and characteristic length of various ice habits,
!     capacitance is equal to 4*pi*(shape factor)
!       see young (1993, pp. 143-152 for guidance)
!
!---- bullets:
!
!---- shape factor for bullets (heymsfield, 1975)
          c_bullet=.5*d
!---- length-width functions for bullets from heymsfield (jas, 1972)
          if (d .gt. 0.3) then
            wd=.25*d**.7856     ! width (mm); a-axis
          else
            wd=.185*d**.552
          endif
!---- characteristic length for bullets (see first multiplicative term on right
!       side of eq. 7 multiplied by crystal width on p. 821 of heymsfield, 1975)
          cl_bullet=.5*pi*wd*(.25*wd+d)/(d+wd)
!
!---- plates:
!
!---- length-width function for plates from heymsfield (jas, 1972)
          wd=.0449*d**.449      ! width or thickness (mm); c-axis
!---- eccentricity & shape factor for thick plates following young (1993, p. 144)
          ecc_plate=sqrt(1.-wd*wd/(d*d))         ! eccentricity
          c_plate=d*ecc_plate/asin(ecc_plate)    ! shape factor
!---- characteristic length for plates following young (1993, p. 150, eq. 6.6)
          cl_plate=d+2.*wd      ! characteristic lengths for plates
!
!---- columns:
!
!---- length-width function for columns from heymsfield (jas, 1972)
          if (d .gt. 0.2) then
            wd=.1973*d**.414    ! width (mm); a-axis
          else
            wd=.5*d             ! width (mm); a-axis
          endif
!---- eccentricity & shape factor for columns following young (1993, p. 144)
          ecc_column=sqrt(1.-wd*wd/(d*d))                     ! eccentricity
          c_column=ecc_column*d/alog((1.+ecc_column)*d/wd)    ! shape factor
!---- characteristic length for columns following young (1993, p. 150, eq. 6.7)
          cl_column=(wd+2.*d)/(c1+c2*d/wd)       ! characteristic lengths for columns
!
!---- convert shape factor & characteristic lengths from mm to m for 
!       ventilation calculations
!
          c_bullet=.001*c_bullet
          c_plate=.001*c_plate
          c_column=.001*c_column
          cl_bullet=.001*cl_bullet
          cl_plate=.001*cl_plate
          cl_column=.001*cl_column
!
!---- make a smooth transition between a ventilation coefficient of 1.0 at 50 microns
!       to 1.1 at 200 microns
!
          if (d .gt. 0.2) then
            cvent1=cvent1i
            cvent2=cvent2i/3.
          else
            cvent1=1.0+.1*max(0., d-.05)/.15
            cvent2=0.
          endif
!
!---- ventilation factors for ice crystals:
!
          vent1(i)=cvent1*(c_bullet+c_plate+c_column)/3.
          vent2(i)=cvent2*(c_bullet*sqrt(v_bullet*cl_bullet)            & 
     &                    +c_plate*sqrt(v_plate*cl_plate)               &
     &                    +c_column*sqrt(v_column*cl_column) )
          crime_best=crime3     ! for calculating best no. of rimed ice crystals
        else
!
!---- this block of code calculates bulk characteristics based on average
!     characteristics of unrimed aggregates >= 1.5 mm using locatelli & 
!     hobbs (jgr, 1974, 2185-2197) data.
!
!----- this category is a composite of aggregates of unrimed radiating 
!-----   assemblages of dendrites or dendrites; aggregates of unrimed
!-----   radiating assemblages of plates, side planes, bullets, & columns;
!-----   aggregates of unrimed side planes (mass in mg, velocity in m/s)
!
          m_agg=(.073*d**1.4+.037*d**1.9+.04*d**1.4)/3.
          v_agg=(.8*d**.16+.69*d**.41+.82*d**.12)/3.
          mass(i)=m_agg
          vel(i)=v_agg
!
!---- assume spherical aggregates
!
!---- shape factor is the same as for bullets, = d/2
          c_agg=.001*.5*d         ! units of m
!---- characteristic length is surface area divided by perimeter
!       (.25*pi*d**2)/(pi*d**2) = d/4
          cl_agg=.5*c_agg         ! units of m
!
!---- ventilation factors for aggregates:
!
          vent1(i)=cvent1a*c_agg
          vent2(i)=cvent2a*c_agg*sqrt(v_agg*cl_agg)
          crime_best=crime4     ! for calculating best no. of rimed aggregates
        endif
!
!---- convert from shape factor to capacitance for ventilation factors
!
        vent1(i)=4.*pi*vent1(i)
        vent2(i)=4.*pi*vent2(i)
        diam(i)=1.e-3*d             ! convert from mm to m
        mass(i)=1.e-6*mass(i)       ! convert from mg to kg
!
!---- calculate increase in fall speeds of individual rimed ice particles
!
        do k=0,nrime
!---- mass of rimed ice particle associated with rime_factor(k)
          rime_m1=rime_factor(k)*mass(i)
          rime_m2=cbulk_ice*diam(i)**3
          m_rime=min(rime_m1, rime_m2)
!---- best number (x) of rimed ice particle combining eqs. (8) & (12) in bohm
          x_rime=crime2*m_rime*(crime_best/m_rime)**.25
!---- reynolds number for rimed ice particle using eq. (11) in bohm
          re_rime=8.5*(sqrt(1.+.1519*sqrt(x_rime))-1.)**2
          rime_vel(k)=crime5*re_rime/diam(i)
        enddo
        do k=1,nrime
          vel_rime(i,k)=rime_vel(k)/rime_vel(0)
        enddo
        if (print_diag) then
   !
   !---- determine if statistics should be printed out.
   !
          iprint=.false.
          if (d .le. 1.) then
            if (mod(i,10) .eq. 0) iprint=.true.
          else
            if (mod(i,100) .eq. 0) iprint=.true.
          endif
          if (iprint) then
            write(6,"(f7.4,5e11.4,1x,5f7.4,1x,2e11.4)")                  &  
     &        d,1.e6*mass(i),m_bullet,m_column,m_plate,m_agg,            &
     &        vel(i),v_bullet,v_column,v_plate,v_agg,                    &
     &        vent1(i),vent2(i)
            write(7,"(f7.4,23f5.2)") d,(vel_rime(i,k), k=1,5),           &
     &        (vel_rime(i,k), k=6,40,2)
            icount=icount+1
          endif
        endif
      enddo
!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!      characteristics as functions of mean particle diameter
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!
      venti1=0.
      venti2=0.
      accri=0.
      massi=0.
      vsnowi=0.
      vel_rf=0.
      ivel_rime=0.
      icount=0
      if (print_diag) then
        icount=60
        write(6,"(/2a)") '------------- statistics as functions of ',    &
     &    'mean particle diameter -------------'
        write(7,"(/2a)") '------ increase in fall speeds of rimed ice',  &
     &    ' particles as functions of mean particle diameter -----'
      endif
      do j=mdimin,mdimax
        if (icount.eq.60 .and. print_diag) then
          write(6,"(/2a)") 'd(mm)    vent1      vent2    ',              &
     &       'accrete       mass     vel  dens  '
          write(7,"(3a)") '      <----------------------------------',   &
     &      '---------------  rime factor  --------------------------',  &
     &      '--------------------------->'
          write(7,"(a,23f5.2)") 'd(mm)',(rime_factor(k), k=1,5),         &
     &       (rime_factor(k), k=6,40,2)
          icount=0
        endif
        mdiam=deldmi*float(j)       ! in m
        smom3=0.
        pratei=0.
        rime_vel=0.                 ! note that this array is being reused!
        do i=idimin,idimax
          dx=diam(i)/mdiam
          if (dx .le. xmax) then    ! to prevent arithmetic underflows
            expf=exp(-dx)*ddeli
            venti1(j)=venti1(j)+vent1(i)*expf
            venti2(j)=venti2(j)+vent2(i)*expf
            accri(j)=accri(j)+diam(i)*diam(i)*vel(i)*expf
            xmass=mass(i)*expf
            do k=1,nrime
              rime_vel(k)=rime_vel(k)+xmass*vel_rime(i,k)
            enddo
            massi(j)=massi(j)+xmass
            pratei=pratei+xmass*vel(i)
            smom3=smom3+diam(i)**3*expf
          else
            exit
          endif
        enddo
   !
   !--- increased fall velocities functions of mean diameter (j),
   !      normalized by ice content, and rime factor (k) 
   !
        do k=1,nrime
          ivel_rime(j,k)=rime_vel(k)/massi(j)
        enddo
   !
   !--- increased fall velocities functions of ice content at 0.1 mm
   !      intervals (j_100) and rime factor (k); accumulations here
   !
        jj=j/100
        if (jj.ge.2 .and. jj.le.9) then
          do k=1,nrime
            vel_rf(jj,k)=vel_rf(jj,k)+ivel_rime(j,k)
          enddo
        endif
        bulk_dens=cbulk*massi(j)/smom3
        venti1(j)=venti1(j)/mdiam
        venti2(j)=venti2(j)/mdiam
        accri(j)=accri(j)/mdiam
        vsnowi(j)=pratei/massi(j)
        massi(j)=massi(j)/mdiam
        if (mod(j,10).eq.0 .and. print_diag) then
          xmdiam=1.e3*mdiam
          write(6,"(f5.3,4e11.4,f6.3,f8.3)") xmdiam,venti1(j),venti2(j), &   
     &      accri(j),massi(j),vsnowi(j),bulk_dens
          write(7,"(f5.3,23f5.2)") xmdiam,(ivel_rime(j,k), k=1,5),       &
     &       (ivel_rime(j,k), k=6,40,2)
          icount=icount+1
        endif
      enddo
!
!--- average increase in fall velocities rimed ice as functions of mean
!      particle diameter (j, only need 0.1 mm intervals) and rime factor (k)
!
      if (print_diag) then
        write(7,"(/2a)") ' ------- increase in fall speeds of rimed ',   & 
     &    'ice particles at reduced, 0.1-mm intervals  --------'
        write(7,"(3a)") '        <----------------------------------',   &
     &    '---------------  rime factor  --------------------------',    &
     &    '--------------------------->'
        write(7,"(a,23f5.2)") 'd(mm)',(rime_factor(k), k=1,5),           &
     &    (rime_factor(k), k=6,40,2)
      endif
      do j=2,9
        vel_rf(j,0)=1.
        do k=1,nrime
          vel_rf(j,k)=.01*vel_rf(j,k)
        enddo
        if (print_diag) write(7,"(f3.1,2x,23f5.2)") 0.1*j,               &
     &    (vel_rf(j,k), k=1,5),(vel_rf(j,k), k=6,40,2)
      enddo
!
!-----------------------------------------------------------------------------------
!
      return
      end
