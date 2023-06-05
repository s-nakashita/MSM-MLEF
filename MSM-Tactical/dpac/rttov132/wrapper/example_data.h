
// Data for example code

// units for gas profiles
int gas_units = 2;   // ppmv over moist air

// datetimes: yy, mm, dd, hh, mm, ss
int datetimes[2][6] = {{2015, 8, 1, 0, 0, 0},
                       {2015, 8, 1, 0, 0, 0}};

// angles: satzen, satazi, sunzen, sunazi
double angles[2][4] = {{0.,  0., 45., 180.},
                       {60., 0., 45., 180.}};

// surftype: surftype, watertype
int surftype[2][2] = {{0, 0},
                      {1, 0}};

// surfgeom: lat, lon, elev
double surfgeom[2][3] = {{10., 20., 0.},
                         {10., 20., 0.}};

// s2m: 2m p, 2m t, 2m q, 10m wind u, v, wind fetch
double s2m[2][6] = {{1013., 0.263178E+03, 0.236131E+04, 4., 2., 100000.},
                    {1013., 0.265178E+03, 0.236131E+04, 4., 2., 100000.}};

// skin: skin T, salinity, snow_frac, foam_frac, fastem_coefsx5
double skin[2][9] = {{270., 35., 0., 0., 3.0, 5.0, 15.0, 0.1, 0.3},
                     {270., 35., 0., 0., 3.0, 5.0, 15.0, 0.1, 0.3}};

// simplecloud: ctp, cfraction
double simplecloud[2][2] = {{500., 0.},
                            {500., 0.}};

// clwscheme: clw_scheme, clwde_param
int clwscheme[2][2]= {{1, 1},
                      {1, 1}};

// icecloud: ice_scheme, icede_param
int icecloud[2][2]= {{2, 1},
                     {2, 1}};

// zeeman: be, cosbk
double zeeman[2][2] = {{0., 0.},
                       {0., 0.}};

double p_ex[101] = {
  5.000000E-03, 1.610000E-02, 3.840000E-02, 7.690000E-02, 1.370000E-01,
  2.244000E-01, 3.454000E-01, 5.064000E-01, 7.140000E-01, 9.753000E-01,
  1.297200E+00, 1.687200E+00, 2.152600E+00, 2.700900E+00, 3.339800E+00,
  4.077000E+00, 4.920400E+00, 5.877600E+00, 6.956700E+00, 8.165500E+00,
  9.511900E+00, 1.100380E+01, 1.264920E+01, 1.445590E+01, 1.643180E+01,
  1.858470E+01, 2.092240E+01, 2.345260E+01, 2.618290E+01, 2.912100E+01,
  3.227440E+01, 3.565050E+01, 3.925660E+01, 4.310010E+01, 4.718820E+01,
  5.152780E+01, 5.612600E+01, 6.098950E+01, 6.612530E+01, 7.153980E+01,
  7.723960E+01, 8.323100E+01, 8.952040E+01, 9.611380E+01, 1.030172E+02,
  1.102366E+02, 1.177775E+02, 1.256456E+02, 1.338462E+02, 1.423848E+02,
  1.512664E+02, 1.604959E+02, 1.700784E+02, 1.800183E+02, 1.903203E+02,
  2.009887E+02, 2.120277E+02, 2.234415E+02, 2.352338E+02, 2.474085E+02,
  2.599691E+02, 2.729191E+02, 2.862617E+02, 3.000000E+02, 3.141369E+02,
  3.286753E+02, 3.436176E+02, 3.589665E+02, 3.747241E+02, 3.908926E+02,
  4.074738E+02, 4.244698E+02, 4.418819E+02, 4.597118E+02, 4.779607E+02,
  4.966298E+02, 5.157200E+02, 5.352322E+02, 5.551669E+02, 5.755248E+02,
  5.963062E+02, 6.175112E+02, 6.391398E+02, 6.611920E+02, 6.836673E+02,
  7.065654E+02, 7.298857E+02, 7.536275E+02, 7.777897E+02, 8.023714E+02,
  8.273713E+02, 8.527880E+02, 8.786201E+02, 9.048659E+02, 9.315236E+02,
  9.585911E+02, 9.860666E+02, 1.013948E+03, 1.042232E+03, 1.070917E+03,
  1.100000E+03};

double t_ex[101] = {
  0.202703E+03,0.210479E+03,0.223526E+03,0.235507E+03,0.245832E+03,
  0.252978E+03,0.256073E+03,0.255984E+03,0.253618E+03,0.249076E+03,
  0.242468E+03,0.237935E+03,0.235849E+03,0.234466E+03,0.232927E+03,
  0.231076E+03,0.228936E+03,0.226858E+03,0.224517E+03,0.221947E+03,
  0.219449E+03,0.218400E+03,0.218000E+03,0.217731E+03,0.217274E+03,
  0.216496E+03,0.215344E+03,0.214555E+03,0.214570E+03,0.215207E+03,
  0.215877E+03,0.216004E+03,0.215823E+03,0.216123E+03,0.216440E+03,
  0.216364E+03,0.216004E+03,0.215577E+03,0.215033E+03,0.214724E+03,
  0.214863E+03,0.215330E+03,0.215942E+03,0.216775E+03,0.217303E+03,
  0.217719E+03,0.217992E+03,0.218271E+03,0.218505E+03,0.218878E+03,
  0.218979E+03,0.218799E+03,0.218879E+03,0.219226E+03,0.219336E+03,
  0.218530E+03,0.216593E+03,0.215502E+03,0.215317E+03,0.215652E+03,
  0.216439E+03,0.217564E+03,0.218817E+03,0.220368E+03,0.222084E+03,
  0.223885E+03,0.225701E+03,0.227594E+03,0.229555E+03,0.231546E+03,
  0.233503E+03,0.235340E+03,0.237075E+03,0.238685E+03,0.240279E+03,
  0.241914E+03,0.243590E+03,0.245288E+03,0.246915E+03,0.248470E+03,
  0.249926E+03,0.251284E+03,0.252683E+03,0.254113E+03,0.255545E+03,
  0.256944E+03,0.258307E+03,0.259679E+03,0.260962E+03,0.261965E+03,
  0.262927E+03,0.264182E+03,0.265441E+03,0.265857E+03,0.264623E+03,
  0.262807E+03,0.263178E+03,0.263178E+03,0.263178E+03,0.263178E+03,
  0.263178E+03};

double q_ex[101] = {
  0.153900E+01,0.186621E+01,0.244113E+01,0.314813E+01,0.413439E+01,
  0.502076E+01,0.544224E+01,0.573639E+01,0.609331E+01,0.620545E+01,
  0.620407E+01,0.614574E+01,0.601924E+01,0.584927E+01,0.568186E+01,
  0.556541E+01,0.543490E+01,0.533202E+01,0.531229E+01,0.530018E+01,
  0.512421E+01,0.482100E+01,0.460633E+01,0.449398E+01,0.448003E+01,
  0.453323E+01,0.453721E+01,0.433437E+01,0.404452E+01,0.383942E+01,
  0.385251E+01,0.399696E+01,0.398998E+01,0.385044E+01,0.381644E+01,
  0.384732E+01,0.385626E+01,0.384055E+01,0.385247E+01,0.389121E+01,
  0.391387E+01,0.396203E+01,0.405763E+01,0.408041E+01,0.410884E+01,
  0.413268E+01,0.416897E+01,0.420164E+01,0.424642E+01,0.426788E+01,
  0.437900E+01,0.458116E+01,0.516925E+01,0.631538E+01,0.788118E+01,
  0.111713E+02,0.183309E+02,0.252613E+02,0.280468E+02,0.349988E+02,
  0.490553E+02,0.695404E+02,0.796122E+02,0.925806E+02,0.970528E+02,
  0.947200E+02,0.102569E+03,0.135846E+03,0.177689E+03,0.227450E+03,
  0.285843E+03,0.365365E+03,0.445904E+03,0.514214E+03,0.586841E+03,
  0.671499E+03,0.770314E+03,0.884119E+03,0.100563E+04,0.113377E+04,
  0.124530E+04,0.134273E+04,0.133982E+04,0.119841E+04,0.973975E+03,
  0.889733E+03,0.103354E+04,0.134580E+04,0.175216E+04,0.227289E+04,
  0.285501E+04,0.330473E+04,0.358459E+04,0.367484E+04,0.329250E+04,
  0.261605E+04,0.263489E+04,0.256225E+04,0.249253E+04,0.242560E+04,
  0.236131E+04};

double co2_ex[101]={
  3.743610E+02, 3.743620E+02, 3.743650E+02, 3.743690E+02, 3.743750E+02,
  3.743850E+02, 3.743970E+02, 3.744130E+02, 3.744430E+02, 3.744910E+02,
  3.745510E+02, 3.746090E+02, 3.746490E+02, 3.746810E+02, 3.747170E+02,
  3.747620E+02, 3.748240E+02, 3.748990E+02, 3.749780E+02, 3.750540E+02,
  3.751160E+02, 3.751650E+02, 3.752080E+02, 3.752400E+02, 3.752750E+02,
  3.753130E+02, 3.753700E+02, 3.754350E+02, 3.755320E+02, 3.756390E+02,
  3.757780E+02, 3.759260E+02, 3.761000E+02, 3.762860E+02, 3.764820E+02,
  3.766890E+02, 3.769300E+02, 3.772320E+02, 3.775510E+02, 3.779150E+02,
  3.783020E+02, 3.786600E+02, 3.789800E+02, 3.793130E+02, 3.795350E+02,
  3.797680E+02, 3.799770E+02, 3.801690E+02, 3.803620E+02, 3.805440E+02,
  3.807340E+02, 3.808930E+02, 3.810480E+02, 3.812020E+02, 3.813590E+02,
  3.815150E+02, 3.816730E+02, 3.818330E+02, 3.819890E+02, 3.821410E+02,
  3.822860E+02, 3.824130E+02, 3.825350E+02, 3.826190E+02, 3.826940E+02,
  3.827520E+02, 3.828030E+02, 3.828430E+02, 3.828800E+02, 3.829120E+02,
  3.829460E+02, 3.829800E+02, 3.830220E+02, 3.830660E+02, 3.831080E+02,
  3.831490E+02, 3.831800E+02, 3.832080E+02, 3.832280E+02, 3.832460E+02,
  3.832580E+02, 3.832720E+02, 3.832840E+02, 3.832990E+02, 3.833180E+02,
  3.833390E+02, 3.833550E+02, 3.833750E+02, 3.834140E+02, 3.834760E+02,
  3.835460E+02, 3.836170E+02, 3.836960E+02, 3.837780E+02, 3.838530E+02,
  3.839160E+02, 3.839670E+02, 3.840030E+02, 3.840270E+02, 3.840360E+02,
  3.840380E+02};
