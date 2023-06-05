subroutine radsim_grib_paramid_name(paramid, idname, icon, typeOfLevel)

integer, intent(in) :: paramid
character(len=*), intent(out) :: idname
logical, intent(in), optional :: icon
character(len=*), intent(in), optional :: typeOfLevel

select case(paramId)
  case(3)
    idname = 'theta'
  case(31, 500069)
    idname = 'seaice'
  case(54, 500001)
    if ( icon .and. trim(typeOfLevel) == 'generalVertical' ) then
      idname = 'ph'
    else
      idname = 'p'
    end if
  case(66)
    idname = 'snow depth'
  case(75, 260020, 500102)
    idname = 'rain'
  case(76, 260021, 500103)
    idname = 'snow'
  case(129)
    idname = 'geopotential'
  case(130, 500014)
    idname = 't'
  case(133, 500035)
    idname = 'q'
  case(134, 500000)
    idname = 'pstar'
  case(152)
    idname = 'log(pstar)'
  case(157)
    idname = 'rh'
  case(164)
    idname = 'total cc'
  case(165, 500027)
    idname = 'u10'
  case(166, 500029)
    idname = 'v10'
  case(167, 500011)
    idname = 't2'
  case(168, 500017)
    idname = 'td2'
  case(172, 500054)
    idname = 'lsm'
  case(186)
    idname = 'low cc'
  case(187)
    idname = 'medium cc'
  case(188)
    idname = 'high cc'
  case(203, 500242, 260624)
    idname = 'o3'
  case(235, 500010)
    idname = 'tskin'
  case(246, 503056)
    idname = 'qcl'
  case(247, 503057)
    idname = 'qcf'
  case(248)
    idname = 'area cloud fraction'
  case(260257, 500098)
    idname = 'cloud cover'
  case(500007)
    idname = 'hsurf'
  case(500779, 503566)
    idname = 'clw deff'
  case(500781, 503568)
    idname = 'ciw deff'
  case(502309, 250003)
    idname = 'latitude'
  case(502310, 250004)
    idname = 'longitude'
  case(3089, 500545)
    idname = 'density'
  case(210001)
    idname = 'cams sea salt bin 1'
  case(210002)
    idname = 'cams sea salt bin 2'
  case(210003)
    idname = 'cams sea salt bin 3'
  case(210004)
    idname = 'cams dust bin 1'
  case(210005)
    idname = 'cams dust bin 2'
  case(210006)
    idname = 'cams dust bin 3'
  case(210007)
    idname = 'cams h-phil org mat'
  case(210010)
    idname = 'cams h-phob blk carb'
  case(210011)
    idname = 'cams sulphate'
  case default
    idname = 'unknown'
end select

end subroutine radsim_grib_paramid_name
