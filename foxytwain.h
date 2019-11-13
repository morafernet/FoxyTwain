*------ CONSTANTES KERNEL32
*------ Constantes HeapCreate, para: flOptions

#DEFINE HEAP_CREATE_ENABLE_EXECUTE		0x00040000
#DEFINE HEAP_GENERATE_EXCEPTIONS		0x00000004
#DEFINE HEAP_NO_SERIALIZE				0x00000001
#DEFINE HEAP_ZERO_MEMORY				0x00000008

*------ Constantes FormatMessage, para: dwFlags
#DEFINE FORMAT_MESSAGE_FROM_SYSTEM		0x00001000
#DEFINE MAX_NAME 						256

*------ Constantes CreateFile, para: dwDesiredAccess
#DEFINE GENERIC_ALL						0x10000000
#DEFINE GENERIC_EXECUTE					0x20000000
#DEFINE GENERIC_WRITE					0x40000000
#DEFINE GENERIC_READ					0x80000000

*------ Constantes CreateFile, para: dwShareMode
#DEFINE FILE_SHARE_OFF					0x00000000
#DEFINE FILE_SHARE_READ					0x00000001
#DEFINE FILE_SHARE_WRITE				0x00000002
#DEFINE FILE_SHARE_DELETE				0x00000004

*------ Constantes CreateFile, para: dwCreationDisposition
#DEFINE CREATE_NEW						1
#DEFINE CREATE_ALWAYS					2
#DEFINE OPEN_EXISTING					3
#DEFINE OPEN_ALWAYS						4
#DEFINE TRUNCATE_EXISTING				5

*------ Constantes CreateFile, para: dwFlagsAndAttributes
#DEFINE FILE_ATTRIBUTE_ARCHIVE			32
#DEFINE FILE_ATTRIBUTE_ENCRYPTED		16384
#DEFINE FILE_ATTRIBUTE_HIDDEN			2
#DEFINE FILE_ATTRIBUTE_NORMAL			128
#DEFINE FILE_ATTRIBUTE_OFFLINE			4096
#DEFINE FILE_ATTRIBUTE_READONLY			1
#DEFINE FILE_ATTRIBUTE_SYSTEM			4
#DEFINE FILE_ATTRIBUTE_TEMPORARY		256
#DEFINE FILE_FLAG_BACKUP_SEMANTICS		0x02000000
#DEFINE FILE_FLAG_DELETE_ON_CLOSE		0x04000000
#DEFINE FILE_FLAG_NO_BUFFERING			0x20000000
#DEFINE FILE_FLAG_OPEN_NO_RECALL		0x00100000
#DEFINE FILE_FLAG_OPEN_REPARSE_POINT	0x00200000
#DEFINE FILE_FLAG_OVERLAPPED			0x40000000
#DEFINE FILE_FLAG_POSIX_SEMANTICS		0x0100000
#DEFINE FILE_FLAG_RANDOM_ACCESS			0x10000000
#DEFINE FILE_FLAG_SESSION_AWARE			0x00800000
#DEFINE FILE_FLAG_SEQUENTIAL_SCAN		0x08000000
#DEFINE FILE_FLAG_WRITE_THROUGH			0x80000000

*------ CONSTANTES TWAIN
*------ Generic Constants
#define TWON_ARRAY						3
#define TWON_ENUMERATION				4
#define TWON_ONEVALUE					5
#define TWON_RANGE						6

#define TWON_ICONID						962
#define TWON_DSMID						461
#define TWON_DSMCODEID					63

#define TWON_DONTCARE8					0xff
#define TWON_DONTCARE16					0xffff
#define TWON_DONTCARE32					0xffffffff

*----- Flags used in TW_MEMORY structure
#define TWMF_APPOWNS					0x0001
#define TWMF_DSMOWNS					0x0002
#define TWMF_DSOWNS						0x0004
#define TWMF_POINTER					0x0008
#define TWMF_HANDLE						0x0010

#define TWTY_INT8						0x0000
#define TWTY_INT16						0x0001
#define TWTY_INT32						0x0002

#define TWTY_UINT8						0x0003
#define TWTY_UINT16						0x0004
#define TWTY_UINT32						0x0005

#define TWTY_BOOL						0x0006

#define TWTY_FIX32						0x0007

#define TWTY_FRAME						0x0008

#define TWTY_STR32						0x0009
#define TWTY_STR64						0x000a
#define TWTY_STR128						0x000b
#define TWTY_STR255						0x000c
#define TWTY_HANDLE						0x000f

*----- Capability Constants
*----- CAP_ALARMS values
#define TWAL_ALARM               0
#define TWAL_FEEDERERROR         1
#define TWAL_FEEDERWARNING       2
#define TWAL_BARCODE             3
#define TWAL_DOUBLEFEED          4
#define TWAL_JAM                 5
#define TWAL_PATCHCODE           6
#define TWAL_POWER               7
#define TWAL_SKEW                8

*----- ICAP_AUTOSIZE values 
#define TWAS_NONE                0
#define TWAS_AUTO                1
#define TWAS_CURRENT             2

*----- TWEI_BARCODEROTATION values 
#define TWBCOR_ROT0              0
#define TWBCOR_ROT90             1
#define TWBCOR_ROT180            2
#define TWBCOR_ROT270            3
#define TWBCOR_ROTX              4

*----- ICAP_BARCODESEARCHMODE values 
#define TWBD_HORZ                0
#define TWBD_VERT                1
#define TWBD_HORZVERT            2
#define TWBD_VERTHORZ            3

*----- ICAP_BITORDER values 
#define TWBO_LSBFIRST            0
#define TWBO_MSBFIRST            1

*----- ICAP_AUTODISCARDBLANKPAGES values 
#define TWBP_DISABLE            -2
#define TWBP_AUTO               -1

*----- ICAP_BITDEPTHREDUCTION values 
#define TWBR_THRESHOLD           0
#define TWBR_HALFTONE            1
#define TWBR_CUSTHALFTONE        2
#define TWBR_DIFFUSION           3
#define TWBR_DYNAMICTHRESHOLD    4

*----- ICAP_SUPPORTEDBARCODETYPES and TWEI_BARCODETYPE values
#define TWBT_3OF9                 0
#define TWBT_2OF5INTERLEAVED      1
#define TWBT_2OF5NONINTERLEAVED   2
#define TWBT_CODE93               3
#define TWBT_CODE128              4
#define TWBT_UCC128               5
#define TWBT_CODABAR              6
#define TWBT_UPCA                 7
#define TWBT_UPCE                 8
#define TWBT_EAN8                 9
#define TWBT_EAN13                10
#define TWBT_POSTNET              11
#define TWBT_PDF417               12
#define TWBT_2OF5INDUSTRIAL       13
#define TWBT_2OF5MATRIX           14
#define TWBT_2OF5DATALOGIC        15
#define TWBT_2OF5IATA             16
#define TWBT_3OF9FULLASCII        17
#define TWBT_CODABARWITHSTARTSTOP 18
#define TWBT_MAXICODE             19
#define TWBT_QRCODE               20

*----- ICAP_COMPRESSION values
#define TWCP_NONE                0
#define TWCP_PACKBITS            1
#define TWCP_GROUP31D            2
#define TWCP_GROUP31DEOL         3
#define TWCP_GROUP32D            4
#define TWCP_GROUP4              5
#define TWCP_JPEG                6
#define TWCP_LZW                 7
#define TWCP_JBIG                8
#define TWCP_PNG                 9
#define TWCP_RLE4               10
#define TWCP_RLE8               11
#define TWCP_BITFIELDS          12
#define TWCP_ZIP                13
#define TWCP_JPEG2000           14

*----- CAP_CAMERASIDE and TWEI_PAGESIDE values 
#define TWCS_BOTH                0
#define TWCS_TOP                 1
#define TWCS_BOTTOM              2

*----- CAP_CLEARBUFFERS values 
#define TWCB_AUTO                0
#define TWCB_CLEAR               1
#define TWCB_NOCLEAR             2

*----- CAP_DEVICEEVENT values 
#define TWDE_CUSTOMEVENTS           0x8000      
#define TWDE_CHECKAUTOMATICCAPTURE  0
#define TWDE_CHECKBATTERY           1
#define TWDE_CHECKDEVICEONLINE      2
#define TWDE_CHECKFLASH             3
#define TWDE_CHECKPOWERSUPPLY       4
#define TWDE_CHECKRESOLUTION        5
#define TWDE_DEVICEADDED            6
#define TWDE_DEVICEOFFLINE          7
#define TWDE_DEVICEREADY            8
#define TWDE_DEVICEREMOVED          9
#define TWDE_IMAGECAPTURED          10
#define TWDE_IMAGEDELETED           11
#define TWDE_PAPERDOUBLEFEED        12
#define TWDE_PAPERJAM               13
#define TWDE_LAMPFAILURE            14
#define TWDE_POWERSAVE              15
#define TWDE_POWERSAVENOTIFY        16

*----- TW_PASSTHRU.Direction values. 
#define TWDR_GET                 1
#define TWDR_SET                 2   

*----- TWEI_DESKEWSTATUS values. 
#define TWDSK_SUCCESS            0
#define TWDSK_REPORTONLY         1
#define TWDSK_FAIL               2
#define TWDSK_DISABLED           3

*----- CAP_DUPLEX values 
#define TWDX_NONE                0
#define TWDX_1PASSDUPLEX         1
#define TWDX_2PASSDUPLEX         2

*----- CAP_FEEDERALIGNMENT values 
#define TWFA_NONE                0
#define TWFA_LEFT                1
#define TWFA_CENTER              2
#define TWFA_RIGHT               3

*----- ICAP_FEEDERTYPE values
#define TWFE_GENERAL             0
#define TWFE_PHOTO               1

*----- ICAP_IMAGEFILEFORMAT values 
#define TWFF_TIFF                0
#define TWFF_PICT                1
#define TWFF_BMP                 2
#define TWFF_XBM                 3
#define TWFF_JFIF                4
#define TWFF_FPX                 5
#define TWFF_TIFFMULTI           6
#define TWFF_PNG                 7
#define TWFF_SPIFF               8
#define TWFF_EXIF                9
#define TWFF_PDF                10
#define TWFF_JP2                11
#define TWFF_JPX                13
#define TWFF_DEJAVU             14
#define TWFF_PDFA               15
#define TWFF_PDFA2              16

*----- ICAP_FLASHUSED2 values 
#define TWFL_NONE                0
#define TWFL_OFF                 1
#define TWFL_ON                  2
#define TWFL_AUTO                3
#define TWFL_REDEYE              4

*----- CAP_FEEDERORDER values 
#define TWFO_FIRSTPAGEFIRST      0
#define TWFO_LASTPAGEFIRST       1

*----- CAP_FEEDERPOCKET values
#define TWFP_POCKETERROR         0
#define TWFP_POCKET1             1
#define TWFP_POCKET2             2
#define TWFP_POCKET3             3
#define TWFP_POCKET4             4
#define TWFP_POCKET5             5
#define TWFP_POCKET6             6
#define TWFP_POCKET7             7
#define TWFP_POCKET8             8
#define TWFP_POCKET9             9
#define TWFP_POCKET10           10
#define TWFP_POCKET11           11
#define TWFP_POCKET12           12
#define TWFP_POCKET13           13
#define TWFP_POCKET14           14
#define TWFP_POCKET15           15
#define TWFP_POCKET16           16

*----- ICAP_FLIPROTATION values 
#define TWFR_BOOK                0
#define TWFR_FANFOLD             1

*----- ICAP_FILTER values 
#define TWFT_RED                 0
#define TWFT_GREEN               1
#define TWFT_BLUE                2
#define TWFT_NONE                3
#define TWFT_WHITE               4
#define TWFT_CYAN                5
#define TWFT_MAGENTA             6
#define TWFT_YELLOW              7
#define TWFT_BLACK               8

*----- TW_FILESYSTEM.FileType values 
#define TWFY_CAMERA              0
#define TWFY_CAMERATOP           1
#define TWFY_CAMERABOTTOM        2
#define TWFY_CAMERAPREVIEW       3
#define TWFY_DOMAIN              4
#define TWFY_HOST                5
#define TWFY_DIRECTORY           6
#define TWFY_IMAGE               7
#define TWFY_UNKNOWN             8

*----- ICAP_ICCPROFILE values  
#define TWIC_NONE                0
#define TWIC_LINK                1
#define TWIC_EMBED               2

*----- ICAP_IMAGEFILTER values 
#define TWIF_NONE                0
#define TWIF_AUTO                1
#define TWIF_LOWPASS             2
#define TWIF_BANDPASS            3
#define TWIF_HIGHPASS            4
#define TWIF_TEXT                TWIF_BANDPASS
#define TWIF_FINELINE            TWIF_HIGHPASS

*----- ICAP_IMAGEMERGE values 
#define TWIM_NONE                0
#define TWIM_FRONTONTOP          1
#define TWIM_FRONTONBOTTOM       2
#define TWIM_FRONTONLEFT         3
#define TWIM_FRONTONRIGHT        4

*----- CAP_JOBCONTROL values  
#define TWJC_NONE                0
#define TWJC_JSIC                1
#define TWJC_JSIS                2
#define TWJC_JSXC                3
#define TWJC_JSXS                4
                  
*----- ICAP_JPEGQUALITY values 
#define TWJQ_UNKNOWN            -4 
#define TWJQ_LOW                -3
#define TWJQ_MEDIUM             -2
#define TWJQ_HIGH               -1

*----- ICAP_LIGHTPATH values 
#define TWLP_REFLECTIVE          0
#define TWLP_TRANSMISSIVE        1

*----- ICAP_LIGHTSOURCE values 
#define TWLS_RED                 0
#define TWLS_GREEN               1
#define TWLS_BLUE                2
#define TWLS_NONE                3
#define TWLS_WHITE               4
#define TWLS_UV                  5
#define TWLS_IR                  6

*----- TWEI_MAGTYPE values 
#define TWMD_MICR                0
#define TWMD_RAW                 1
#define TWMD_INVALID             2

*----- ICAP_NOISEFILTER values 
#define TWNF_NONE                0
#define TWNF_AUTO                1
#define TWNF_LONEPIXEL           2
#define TWNF_MAJORITYRULE        3

*----- ICAP_ORIENTATION values 
#define TWOR_ROT0                0
#define TWOR_ROT90               1
#define TWOR_ROT180              2
#define TWOR_ROT270              3
#define TWOR_PORTRAIT            TWOR_ROT0
#define TWOR_LANDSCAPE           TWOR_ROT270
#define TWOR_AUTO                4
#define TWOR_AUTOTEXT            5
#define TWOR_AUTOPICTURE         6

*----- ICAP_OVERSCAN values
#define TWOV_NONE                0
#define TWOV_AUTO                1
#define TWOV_TOPBOTTOM           2
#define TWOV_LEFTRIGHT           3
#define TWOV_ALL                 4

*----- Palette types for TW_PALETTE8
#define TWPA_RGB				0
#define TWPA_GRAY				1
#define TWPA_CMY				2

*----- ICAP_PLANARCHUNKY values
#define TWPC_CHUNKY              0
#define TWPC_PLANAR              1

*----- TWEI_PATCHCODE values
#define TWPCH_PATCH1             0
#define TWPCH_PATCH2             1
#define TWPCH_PATCH3             2
#define TWPCH_PATCH4             3
#define TWPCH_PATCH6             4
#define TWPCH_PATCHT             5

*----- ICAP_PIXELFLAVOR values
#define TWPF_CHOCOLATE           0
#define TWPF_VANILLA             1

*----- CAP_PRINTERMODE values
#define TWPM_SINGLESTRING        0
#define TWPM_MULTISTRING         1
#define TWPM_COMPOUNDSTRING      2

*----- CAP_PRINTER values 
#define TWPR_IMPRINTERTOPBEFORE     0
#define TWPR_IMPRINTERTOPAFTER      1
#define TWPR_IMPRINTERBOTTOMBEFORE  2
#define TWPR_IMPRINTERBOTTOMAFTER   3
#define TWPR_ENDORSERTOPBEFORE      4
#define TWPR_ENDORSERTOPAFTER       5
#define TWPR_ENDORSERBOTTOMBEFORE   6
#define TWPR_ENDORSERBOTTOMAFTER    7

*----- CAP_PRINTERFONTSTYLE Added 2.3 
#define TWPF_NORMAL              0
#define TWPF_BOLD                1
#define TWPF_ITALIC              2
#define TWPF_LARGESIZE           3
#define TWPF_SMALLSIZE           4

*----- CAP_PRINTERINDEXTRIGGER Added 2.3 
#define TWCT_PAGE                0
#define TWCT_PATCH1              1
#define TWCT_PATCH2              2
#define TWCT_PATCH3              3
#define TWCT_PATCH4              4
#define TWCT_PATCHT              5
#define TWCT_PATCH6              6

*----- CAP_POWERSUPPLY values 
#define TWPS_EXTERNAL            0
#define TWPS_BATTERY             1

*----- ICAP_PIXELTYPE values (PT_ means Pixel Type) 
#define TWPT_BW                  0
#define TWPT_GRAY                1
#define TWPT_RGB                 2
#define TWPT_PALETTE             3
#define TWPT_CMY                 4
#define TWPT_CMYK                5
#define TWPT_YUV                 6
#define TWPT_YUVK                7
#define TWPT_CIEXYZ              8
#define TWPT_LAB                 9
#define TWPT_SRGB               10
#define TWPT_SCRGB              11
#define TWPT_INFRARED           16

*------ CAP_SEGMENTED values 
#define TWSG_NONE                0
#define TWSG_AUTO                1
#define TWSG_MANUAL              2

*------ ICAP_FILMTYPE values 
#define TWFM_POSITIVE            0
#define TWFM_NEGATIVE            1

*----- CAP_DOUBLEFEEDDETECTION 
#define TWDF_ULTRASONIC          0
#define TWDF_BYLENGTH            1
#define TWDF_INFRARED            2

*----- CAP_DOUBLEFEEDDETECTIONSENSITIVITY 
#define TWUS_LOW                 0
#define TWUS_MEDIUM              1
#define TWUS_HIGH                2

*----- CAP_DOUBLEFEEDDETECTIONRESPONSE 
#define TWDP_STOP                 0
#define TWDP_STOPANDWAIT          1
#define TWDP_SOUND                2
#define TWDP_DONOTIMPRINT         3

*----- ICAP_MIRROR values 
#define TWMR_NONE                 0
#define TWMR_VERTICAL             1
#define TWMR_HORIZONTAL           2

*----- ICAP_JPEGSUBSAMPLING values 
#define TWJS_444YCBCR            0
#define TWJS_444RGB              1
#define TWJS_422                 2
#define TWJS_421                 3
#define TWJS_411                 4
#define TWJS_420                 5
#define TWJS_410                 6
#define TWJS_311                 7

*----- CAP_PAPERHANDLING values 
#define TWPH_NORMAL              0
#define TWPH_FRAGILE             1
#define TWPH_THICK               2
#define TWPH_TRIFOLD             3
#define TWPH_PHOTOGRAPH          4

*----- CAP_INDICATORSMODE values 
#define TWCI_INFO                0
#define TWCI_WARNING             1
#define TWCI_ERROR               2
#define TWCI_WARMUP              3

*----- ICAP_SUPPORTEDSIZES values (SS_ means Supported Sizes) 
#define TWSS_NONE                0
#define TWSS_A4                  1
#define TWSS_JISB5               2
#define TWSS_USLETTER            3
#define TWSS_USLEGAL             4
#define TWSS_A5                  5
#define TWSS_ISOB4               6
#define TWSS_ISOB6               7
#define TWSS_USLEDGER            9
#define TWSS_USEXECUTIVE        10
#define TWSS_A3                 11
#define TWSS_ISOB3              12
#define TWSS_A6                 13
#define TWSS_C4                 14
#define TWSS_C5                 15
#define TWSS_C6                 16
#define TWSS_4A0                17
#define TWSS_2A0                18
#define TWSS_A0                 19
#define TWSS_A1                 20
#define TWSS_A2                 21
#define TWSS_A7                 22
#define TWSS_A8                 23
#define TWSS_A9                 24
#define TWSS_A10                25
#define TWSS_ISOB0              26
#define TWSS_ISOB1              27
#define TWSS_ISOB2              28
#define TWSS_ISOB5              29
#define TWSS_ISOB7              30
#define TWSS_ISOB8              31
#define TWSS_ISOB9              32
#define TWSS_ISOB10             33
#define TWSS_JISB0              34
#define TWSS_JISB1              35
#define TWSS_JISB2              36
#define TWSS_JISB3              37
#define TWSS_JISB4              38
#define TWSS_JISB6              39
#define TWSS_JISB7              40
#define TWSS_JISB8              41
#define TWSS_JISB9              42
#define TWSS_JISB10             43
#define TWSS_C0                 44
#define TWSS_C1                 45
#define TWSS_C2                 46
#define TWSS_C3                 47
#define TWSS_C7                 48
#define TWSS_C8                 49
#define TWSS_C9                 50
#define TWSS_C10                51
#define TWSS_USSTATEMENT        52
#define TWSS_BUSINESSCARD       53
#define TWSS_MAXSIZE            54

*----- ICAP_XFERMECH values (SX_ means Setup XFer) 
#define TWSX_NATIVE              0
#define TWSX_FILE                1
#define TWSX_MEMORY              2
#define TWSX_MEMFILE             4

*----- ICAP_UNITS values (UN_ means UNits) 
#define TWUN_INCHES              0
#define TWUN_CENTIMETERS         1
#define TWUN_PICAS               2
#define TWUN_POINTS              3
#define TWUN_TWIPS               4
#define TWUN_PIXELS              5
#define TWUN_MILLIMETERS         6

*----- COUNTRY CONSTANTS
#define TWCY_AFGHANISTAN				1001
#define TWCY_ALGERIA					213
#define TWCY_AMERICANSAMOA				684
#define TWCY_ANDORRA					033
#define TWCY_ANGOLA						1002
#define TWCY_ANGUILLA					8090
#define TWCY_ANTIGUA					8091
#define TWCY_ARGENTINA					54
#define TWCY_ARUBA						297
#define TWCY_ASCENSIONI					247
#define TWCY_AUSTRALIA					61
#define TWCY_AUSTRIA					43
#define TWCY_BAHAMAS					8092
#define TWCY_BAHRAIN					973
#define TWCY_BANGLADESH					880
#define TWCY_BARBADOS      8093
#define TWCY_BELGIUM         32
#define TWCY_BELIZE         501
#define TWCY_BENIN          229
#define TWCY_BERMUDA       8094
#define TWCY_BHUTAN        1003
#define TWCY_BOLIVIA        591
#define TWCY_BOTSWANA       267
#define TWCY_BRITAIN          6
#define TWCY_BRITVIRGINIS  8095
#define TWCY_BRAZIL          55
#define TWCY_BRUNEI         673
#define TWCY_BULGARIA       359
#define TWCY_BURKINAFASO   1004
#define TWCY_BURMA         1005
#define TWCY_BURUNDI       1006
#define TWCY_CAMAROON       237
#define TWCY_CANADA           2
#define TWCY_CAPEVERDEIS    238
#define TWCY_CAYMANIS      8096
#define TWCY_CENTRALAFREP  1007
#define TWCY_CHAD          1008
#define TWCY_CHILE           56
#define TWCY_CHINA           86
#define TWCY_CHRISTMASIS   1009
#define TWCY_COCOSIS       1009
#define TWCY_COLOMBIA        57
#define TWCY_COMOROS       1010
#define TWCY_CONGO         1011
#define TWCY_COOKIS        1012
#define TWCY_COSTARICA     506
#define TWCY_CUBA           005
#define TWCY_CYPRUS         357
#define TWCY_CZECHOSLOVAKIA  42
#define TWCY_DENMARK         45
#define TWCY_DJIBOUTI      1013
#define TWCY_DOMINICA      8097
#define TWCY_DOMINCANREP   8098
#define TWCY_EASTERIS      1014
#define TWCY_ECUADOR        593
#define TWCY_EGYPT           20
#define TWCY_ELSALVADOR     503
#define TWCY_EQGUINEA      1015
#define TWCY_ETHIOPIA       251
#define TWCY_FALKLANDIS    1016
#define TWCY_FAEROEIS       298
#define TWCY_FIJIISLANDS    679
#define TWCY_FINLAND        358
#define TWCY_FRANCE          33
#define TWCY_FRANTILLES     596
#define TWCY_FRGUIANA       594
#define TWCY_FRPOLYNEISA    689
#define TWCY_FUTANAIS      1043
#define TWCY_GABON          241
#define TWCY_GAMBIA         220
#define TWCY_GERMANY         49
#define TWCY_GHANA          233
#define TWCY_GIBRALTER      350
#define TWCY_GREECE          30
#define TWCY_GREENLAND      299
#define TWCY_GRENADA       8099
#define TWCY_GRENEDINES    8015
#define TWCY_GUADELOUPE     590
#define TWCY_GUAM           671
#define TWCY_GUANTANAMOBAY 5399
#define TWCY_GUATEMALA      502
#define TWCY_GUINEA         224
#define TWCY_GUINEABISSAU  1017
#define TWCY_GUYANA         592
#define TWCY_HAITI          509
#define TWCY_HONDURAS       504
#define TWCY_HONGKONG      852
#define TWCY_HUNGARY         36
#define TWCY_ICELAND        354
#define TWCY_INDIA           91
#define TWCY_INDONESIA       62
#define TWCY_IRAN            98
#define TWCY_IRAQ           964
#define TWCY_IRELAND        353
#define TWCY_ISRAEL         972
#define TWCY_ITALY           39
#define TWCY_IVORYCOAST    225
#define TWCY_JAMAICA       8010
#define TWCY_JAPAN           81
#define TWCY_JORDAN         962
#define TWCY_KENYA          254
#define TWCY_KIRIBATI      1018
#define TWCY_KOREA           82
#define TWCY_KUWAIT         965
#define TWCY_LAOS          1019
#define TWCY_LEBANON       1020
#define TWCY_LIBERIA        231
#define TWCY_LIBYA          218
#define TWCY_LIECHTENSTEIN   41
#define TWCY_LUXENBOURG     352
#define TWCY_MACAO          853
#define TWCY_MADAGASCAR    1021
#define TWCY_MALAWI         265
#define TWCY_MALAYSIA        60
#define TWCY_MALDIVES       960
#define TWCY_MALI          1022
#define TWCY_MALTA          356
#define TWCY_MARSHALLIS     692
#define TWCY_MAURITANIA    1023
#define TWCY_MAURITIUS      230
#define TWCY_MEXICO           3
#define TWCY_MICRONESIA     691
#define TWCY_MIQUELON       508
#define TWCY_MONACO          33
#define TWCY_MONGOLIA      1024
#define TWCY_MONTSERRAT    8011
#define TWCY_MOROCCO        212
#define TWCY_MOZAMBIQUE    1025
#define TWCY_NAMIBIA        264
#define TWCY_NAURU         1026
#define TWCY_NEPAL          977
#define TWCY_NETHERLANDS     31
#define TWCY_NETHANTILLES   599
#define TWCY_NEVIS         8012
#define TWCY_NEWCALEDONIA   687
#define TWCY_NEWZEALAND      64
#define TWCY_NICARAGUA      505
#define TWCY_NIGER          227
#define TWCY_NIGERIA        234
#define TWCY_NIUE          1027
#define TWCY_NORFOLKI      1028
#define TWCY_NORWAY          47
#define TWCY_OMAN           968
#define TWCY_PAKISTAN        92
#define TWCY_PALAU         1029
#define TWCY_PANAMA         507
#define TWCY_PARAGUAY       595
#define TWCY_PERU            51
#define TWCY_PHILLIPPINES    63
#define TWCY_PITCAIRNIS    1030
#define TWCY_PNEWGUINEA     675
#define TWCY_POLAND          48
#define TWCY_PORTUGAL       351
#define TWCY_QATAR          974
#define TWCY_REUNIONI      1031
#define TWCY_ROMANIA         40
#define TWCY_RWANDA         250
#define TWCY_SAIPAN         670
#define TWCY_SANMARINO       39
#define TWCY_SAOTOME       1033
#define TWCY_SAUDIARABIA    966
#define TWCY_SENEGAL        221
#define TWCY_SEYCHELLESIS  1034
#define TWCY_SIERRALEONE   1035
#define TWCY_SINGAPORE       65
#define TWCY_SOLOMONIS     1036
#define TWCY_SOMALI        1037
#define TWCY_SOUTHAFRICA    27
#define TWCY_SPAIN           34
#define TWCY_SRILANKA        94
#define TWCY_STHELENA      1032
#define TWCY_STKITTS       8013
#define TWCY_STLUCIA       8014
#define TWCY_STPIERRE       508
#define TWCY_STVINCENT     8015
#define TWCY_SUDAN         1038
#define TWCY_SURINAME       597
#define TWCY_SWAZILAND      268
#define TWCY_SWEDEN          46
#define TWCY_SWITZERLAND     41
#define TWCY_SYRIA         1039
#define TWCY_TAIWAN         886
#define TWCY_TANZANIA       255
#define TWCY_THAILAND        66
#define TWCY_TOBAGO        8016
#define TWCY_TOGO           228
#define TWCY_TONGAIS        676
#define TWCY_TRINIDAD      8016
#define TWCY_TUNISIA        216
#define TWCY_TURKEY          90
#define TWCY_TURKSCAICOS   8017
#define TWCY_TUVALU        1040
#define TWCY_UGANDA         256
#define TWCY_USSR             7
#define TWCY_UAEMIRATES     971
#define TWCY_UNITEDKINGDOM   44
#define TWCY_USA              1
#define TWCY_URUGUAY        598
#define TWCY_VANUATU       1041
#define TWCY_VATICANCITY     39
#define TWCY_VENEZUELA       58
#define TWCY_WAKE          1042
#define TWCY_WALLISIS      1043
#define TWCY_WESTERNSAHARA 1044
#define TWCY_WESTERNSAMOA  1045
#define TWCY_YEMEN         1046
#define TWCY_YUGOSLAVIA      38
#define TWCY_ZAIRE          243
#define TWCY_ZAMBIA         260
#define TWCY_ZIMBABWE       263
#define TWCY_ALBANIA        355
#define TWCY_ARMENIA        374
#define TWCY_AZERBAIJAN     994
#define TWCY_BELARUS        375
#define TWCY_BOSNIAHERZGO   387
#define TWCY_CAMBODIA       855
#define TWCY_CROATIA        385
#define TWCY_CZECHREPUBLIC  420
#define TWCY_DIEGOGARCIA    246
#define TWCY_ERITREA        291
#define TWCY_ESTONIA        372
#define TWCY_GEORGIA        995
#define TWCY_LATVIA         371
#define TWCY_LESOTHO        266
#define TWCY_LITHUANIA      370
#define TWCY_MACEDONIA      389
#define TWCY_MAYOTTEIS      269
#define TWCY_MOLDOVA        373
#define TWCY_MYANMAR        95
#define TWCY_NORTHKOREA     850
#define TWCY_PUERTORICO     787
#define TWCY_RUSSIA         7
#define TWCY_SERBIA         381
#define TWCY_SLOVAKIA       421
#define TWCY_SLOVENIA       386
#define TWCY_SOUTHKOREA     82
#define TWCY_UKRAINE        380
#define TWCY_USVIRGINIS     340
#define TWCY_VIETNAM        84

*----- LANGUAGE CONSTANTS
#define TWLG_USERLOCALE           -1
#define TWLG_DAN                   0
#define TWLG_DUT                   1
#define TWLG_ENG                   2
#define TWLG_FCF                   3
#define TWLG_FIN                   4
#define TWLG_FRN                   5
#define TWLG_GER                   6
#define TWLG_ICE                   7
#define TWLG_ITN                   8
#define TWLG_NOR                   9
#define TWLG_POR                   10
#define TWLG_SPA                   11
#define TWLG_SWE                   12
#define TWLG_USA                   13
#define TWLG_AFRIKAANS             14
#define TWLG_ALBANIA               15
#define TWLG_ARABIC                16
#define TWLG_ARABIC_ALGERIA        17
#define TWLG_ARABIC_BAHRAIN        18
#define TWLG_ARABIC_EGYPT          19
#define TWLG_ARABIC_IRAQ           20
#define TWLG_ARABIC_JORDAN         21
#define TWLG_ARABIC_KUWAIT         22
#define TWLG_ARABIC_LEBANON        23
#define TWLG_ARABIC_LIBYA          24
#define TWLG_ARABIC_MOROCCO        25
#define TWLG_ARABIC_OMAN           26
#define TWLG_ARABIC_QATAR          27
#define TWLG_ARABIC_SAUDIARABIA    28
#define TWLG_ARABIC_SYRIA          29
#define TWLG_ARABIC_TUNISIA        30
#define TWLG_ARABIC_UAE            31
#define TWLG_ARABIC_YEMEN          32
#define TWLG_BASQUE                33
#define TWLG_BYELORUSSIAN          34
#define TWLG_BULGARIAN             35
#define TWLG_CATALAN               36
#define TWLG_CHINESE               37
#define TWLG_CHINESE_HONGKONG      38
#define TWLG_CHINESE_PRC           39
#define TWLG_CHINESE_SINGAPORE     40
#define TWLG_CHINESE_SIMPLIFIED    41
#define TWLG_CHINESE_TAIWAN        42
#define TWLG_CHINESE_TRADITIONAL   43
#define TWLG_CROATIA               44
#define TWLG_CZECH                 45
#define TWLG_DANISH                TWLG_DAN
#define TWLG_DUTCH                 TWLG_DUT
#define TWLG_DUTCH_BELGIAN         46
#define TWLG_ENGLISH               TWLG_ENG
#define TWLG_ENGLISH_AUSTRALIAN    47
#define TWLG_ENGLISH_CANADIAN      48
#define TWLG_ENGLISH_IRELAND       49
#define TWLG_ENGLISH_NEWZEALAND    50
#define TWLG_ENGLISH_SOUTHAFRICA   51
#define TWLG_ENGLISH_UK            52
#define TWLG_ENGLISH_USA           TWLG_USA
#define TWLG_ESTONIAN              53
#define TWLG_FAEROESE              54
#define TWLG_FARSI                 55
#define TWLG_FINNISH               TWLG_FIN
#define TWLG_FRENCH                TWLG_FRN
#define TWLG_FRENCH_BELGIAN        56
#define TWLG_FRENCH_CANADIAN       TWLG_FCF
#define TWLG_FRENCH_LUXEMBOURG     57
#define TWLG_FRENCH_SWISS          58
#define TWLG_GERMAN                TWLG_GER
#define TWLG_GERMAN_AUSTRIAN       59
#define TWLG_GERMAN_LUXEMBOURG     60
#define TWLG_GERMAN_LIECHTENSTEIN  61
#define TWLG_GERMAN_SWISS          62
#define TWLG_GREEK                 63
#define TWLG_HEBREW                64
#define TWLG_HUNGARIAN             65
#define TWLG_ICELANDIC             TWLG_ICE
#define TWLG_INDONESIAN            66
#define TWLG_ITALIAN               TWLG_ITN
#define TWLG_ITALIAN_SWISS         67
#define TWLG_JAPANESE              68
#define TWLG_KOREAN                69
#define TWLG_KOREAN_JOHAB          70
#define TWLG_LATVIAN               71
#define TWLG_LITHUANIAN            72
#define TWLG_NORWEGIAN             TWLG_NOR
#define TWLG_NORWEGIAN_BOKMAL      73
#define TWLG_NORWEGIAN_NYNORSK     74
#define TWLG_POLISH                75
#define TWLG_PORTUGUESE            TWLG_POR
#define TWLG_PORTUGUESE_BRAZIL     76
#define TWLG_ROMANIAN              77
#define TWLG_RUSSIAN               78
#define TWLG_SERBIAN_LATIN         79
#define TWLG_SLOVAK                80
#define TWLG_SLOVENIAN             81
#define TWLG_SPANISH               TWLG_SPA
#define TWLG_SPANISH_MEXICAN       82
#define TWLG_SPANISH_MODERN        83
#define TWLG_SWEDISH               TWLG_SWE
#define TWLG_THAI                  84
#define TWLG_TURKISH               85
#define TWLG_UKRANIAN              86
#define TWLG_ASSAMESE              87
#define TWLG_BENGALI               88
#define TWLG_BIHARI                89
#define TWLG_BODO                  90
#define TWLG_DOGRI                 91
#define TWLG_GUJARATI              92
#define TWLG_HARYANVI              93
#define TWLG_HINDI                 94
#define TWLG_KANNADA               95
#define TWLG_KASHMIRI              96
#define TWLG_MALAYALAM             97
#define TWLG_MARATHI               98
#define TWLG_MARWARI               99
#define TWLG_MEGHALAYAN            100
#define TWLG_MIZO                  101
#define TWLG_NAGA                  102
#define TWLG_ORISSI                103
#define TWLG_PUNJABI               104
#define TWLG_PUSHTU                105
#define TWLG_SERBIAN_CYRILLIC      106
#define TWLG_SIKKIMI               107
#define TWLG_SWEDISH_FINLAND       108
#define TWLG_TAMIL                 109
#define TWLG_TELUGU                110
#define TWLG_TRIPURI               111
#define TWLG_URDU                  112
#define TWLG_VIETNAMESE            113

*----- Data Groups
#define DG_CONTROL          0x0001
#define DG_IMAGE            0x0002
#define DG_AUDIO            0x0004 

#define DF_DSM2             0x10000000
#define DF_APP2             0x20000000

#define DF_DS2              0x40000000

#define DG_MASK             0xFFFF

*----- Constantes DAT
#define DAT_NULL            0x0000
#define DAT_CUSTOMBASE      0x8000

*----- Tipos de argumentos de datos para el grupo de datos DG_CONTROL.
#define DAT_CAPABILITY					0x0001
#define DAT_EVENT						0x0002
#define DAT_IDENTITY					0x0003
#define DAT_PARENT						0x0004
#define DAT_PENDINGXFERS				0x0005
#define DAT_SETUPMEMXFER				0x0006
#define DAT_SETUPFILEXFER				0x0007
#define DAT_STATUS						0x0008
#define DAT_USERINTERFACE				0x0009
#define DAT_XFERGROUP					0x000a
#define DAT_CUSTOMDSDATA				0x000c
#define DAT_DEVICEEVENT					0x000d
#define DAT_FILESYSTEM					0x000e
#define DAT_PASSTHRU					0x000f
#define DAT_CALLBACK					0x0010
#define DAT_STATUSUTF8					0x0011
#define DAT_CALLBACK2					0x0012

*----- Data Argument Types for the DG_IMAGE Data Group.
#define DAT_IMAGEINFO					0x0101
#define DAT_IMAGELAYOUT					0x0102
#define DAT_IMAGEMEMXFER				0x0103
#define DAT_IMAGENATIVEXFER				0x0104
#define DAT_IMAGEFILEXFER				0x0105
#define DAT_CIECOLOR					0x0106
#define DAT_GRAYRESPONSE				0x0107
#define DAT_RGBRESPONSE					0x0108
#define DAT_JPEGCOMPRESSION				0x0109
#define DAT_PALETTE8					0x010a
#define DAT_EXTIMAGEINFO				0x010b
#define DAT_FILTER						0x010c

*----- Data Argument Types for the DG_AUDIO Data Group.
#define DAT_AUDIOFILEXFER				0x0201
#define DAT_AUDIOINFO					0x0202
#define DAT_AUDIONATIVEXFER				0x0203

*----- misplaced
#define DAT_ICCPROFILE					0x0401
#define DAT_IMAGEMEMFILEXFER			0x0402
#define DAT_ENTRYPOINT					0x0403






#DEFINE TWON_PROTOCOLMAJOR				1
#DEFINE TWON_ONEVALUE					5
#DEFINE TWON_PROTOCOLMINOR				9

*----- All message constants are unique.
*----- Messages are grouped according to which DATs they are used with.
#define MSG_NULL						0x0000
#define MSG_CUSTOMBASE					0x8000

*----- Mensajes genericos que pueden ser usado con varios DATs.
#define MSG_GET							0x0001
#define MSG_GETCURRENT					0x0002
#define MSG_GETDEFAULT					0x0003
#define MSG_GETFIRST					0x0004
#define MSG_GETNEXT						0x0005
#define MSG_SET							0x0006
#define MSG_RESET						0x0007
#define MSG_QUERYSUPPORT				0x0008
#define MSG_GETHELP						0x0009
#define MSG_GETLABEL					0x000a
#define MSG_GETLABELENUM				0x000b
#define MSG_SETCONSTRAINT				0x000c

*----- Messages used with DAT_NULL
#define MSG_XFERREADY					0x0101
#define MSG_CLOSEDSREQ					0x0102
#define MSG_CLOSEDSOK					0x0103
#define MSG_DEVICEEVENT					0X0104

*----- Messages used with a pointer to DAT_PARENT data
#define MSG_OPENDSM						0x0301
#define MSG_CLOSEDSM					0x0302

*----- Messages used with a pointer to a DAT_IDENTITY structure
#define MSG_OPENDS						0x0401
#define MSG_CLOSEDS						0x0402
#define MSG_USERSELECT					0x0403

*----- Messages used with a pointer to a DAT_USERINTERFACE structure
#define MSG_DISABLEDS					0x0501
#define MSG_ENABLEDS					0x0502
#define MSG_ENABLEDSUIONLY				0x0503

*----- Messages used with a pointer to a DAT_EVENT structure
#define MSG_PROCESSEVENT				0x0601

*----- Messages used with a pointer to a DAT_PENDINGXFERS structure
#define MSG_ENDXFER						0x0701
#define MSG_STOPFEEDER					0x0702

*----- Messages used with a pointer to a DAT_FILESYSTEM structure
#define MSG_CHANGEDIRECTORY				0x0801
#define MSG_CREATEDIRECTORY				0x0802
#define MSG_DELETE						0x0803
#define MSG_FORMATMEDIA					0x0804
#define MSG_GETCLOSE					0x0805
#define MSG_GETFIRSTFILE				0x0806
#define MSG_GETINFO						0x0807
#define MSG_GETNEXTFILE					0x0808
#define MSG_RENAME						0x0809
#define MSG_COPY						0x080A
#define MSG_AUTOMATICCAPTUREDIRECTORY	0x080B

*----- Messages used with a pointer to a DAT_PASSTHRU structure
#define MSG_PASSTHRU					0x0901

*----- used with DAT_CALLBACK
#define MSG_REGISTER_CALLBACK			0x0902

*----- used with DAT_CAPABILITY
#define MSG_RESETALL					0x0A01

*----- CAPABILITIES
*----- Base of custom capabilities
#define CAP_CUSTOMBASE          0x8000

*----- all data sources are REQUIRED to support these caps 
#define CAP_XFERCOUNT           0x0001

*----- image data sources are REQUIRED to support these caps 
#define ICAP_COMPRESSION        0x0100
#define ICAP_PIXELTYPE          0x0101
#define ICAP_UNITS              0x0102
#define ICAP_XFERMECH           0x0103

*----- all data sources MAY support these caps 
#define CAP_AUTHOR							0x1000
#define CAP_CAPTION							0x1001
#define CAP_FEEDERENABLED					0x1002
#define CAP_FEEDERLOADED					0x1003
#define CAP_TIMEDATE						0x1004
#define CAP_SUPPORTEDCAPS					0x1005
#define CAP_EXTENDEDCAPS					0x1006
#define CAP_AUTOFEED						0x1007
#define CAP_CLEARPAGE						0x1008
#define CAP_FEEDPAGE						0x1009
#define CAP_REWINDPAGE						0x100a
#define CAP_INDICATORS						0x100b
#define CAP_PAPERDETECTABLE					0x100d
#define CAP_UICONTROLLABLE					0x100e
#define CAP_DEVICEONLINE					0x100f
#define CAP_AUTOSCAN						0x1010
#define CAP_THUMBNAILSENABLED				0x1011
#define CAP_DUPLEX							0x1012
#define CAP_DUPLEXENABLED					0x1013
#define CAP_ENABLEDSUIONLY					0x1014
#define CAP_CUSTOMDSDATA					0x1015
#define CAP_ENDORSER						0x1016
#define CAP_JOBCONTROL						0x1017
#define CAP_ALARMS							0x1018
#define CAP_ALARMVOLUME						0x1019
#define CAP_AUTOMATICCAPTURE				0x101a
#define CAP_TIMEBEFOREFIRSTCAPTURE			0x101b
#define CAP_TIMEBETWEENCAPTURES				0x101c
#define CAP_CLEARBUFFERS					0x101d
#define CAP_MAXBATCHBUFFERS         0x101e
#define CAP_DEVICETIMEDATE          0x101f
#define CAP_POWERSUPPLY             0x1020
#define CAP_CAMERAPREVIEWUI         0x1021
#define CAP_DEVICEEVENT             0x1022
#define CAP_SERIALNUMBER            0x1024
#define CAP_PRINTER                 0x1026
#define CAP_PRINTERENABLED          0x1027
#define CAP_PRINTERINDEX            0x1028
#define CAP_PRINTERMODE             0x1029
#define CAP_PRINTERSTRING           0x102a
#define CAP_PRINTERSUFFIX           0x102b
#define CAP_LANGUAGE                0x102c
#define CAP_FEEDERALIGNMENT         0x102d
#define CAP_FEEDERORDER             0x102e
#define CAP_REACQUIREALLOWED        0x1030
#define CAP_BATTERYMINUTES          0x1032
#define CAP_BATTERYPERCENTAGE       0x1033
#define CAP_CAMERASIDE              0x1034
#define CAP_SEGMENTED               0x1035
#define CAP_CAMERAENABLED           0x1036
#define CAP_CAMERAORDER             0x1037
#define CAP_MICRENABLED             0x1038
#define CAP_FEEDERPREP              0x1039
#define CAP_FEEDERPOCKET            0x103a
#define CAP_AUTOMATICSENSEMEDIUM    0x103b
#define CAP_CUSTOMINTERFACEGUID     0x103c
#define CAP_SUPPORTEDCAPSSEGMENTUNIQUE    0x103d
#define CAP_SUPPORTEDDATS           0x103e
#define CAP_DOUBLEFEEDDETECTION     0x103f
#define CAP_DOUBLEFEEDDETECTIONLENGTH 0x1040
#define CAP_DOUBLEFEEDDETECTIONSENSITIVITY 0x1041
#define CAP_DOUBLEFEEDDETECTIONRESPONSE 0x1042
#define CAP_PAPERHANDLING           0x1043
#define CAP_INDICATORSMODE          0x1044
#define CAP_PRINTERVERTICALOFFSET   0x1045
#define CAP_POWERSAVETIME           0x1046
#define CAP_PRINTERCHARROTATION	    0x1047
#define CAP_PRINTERFONTSTYLE        0x1048
#define CAP_PRINTERINDEXLEADCHAR    0x1049
#define CAP_PRINTERINDEXMAXVALUE    0x104A
#define CAP_PRINTERINDEXNUMDIGITS   0x104B
#define CAP_PRINTERINDEXSTEP        0x104C
#define CAP_PRINTERINDEXTRIGGER     0x104D
#define CAP_PRINTERSTRINGPREVIEW    0x104E



*----- image data sources MAY support these caps 
#define ICAP_AUTOBRIGHT                   0x1100
#define ICAP_BRIGHTNESS                   0x1101
#define ICAP_CONTRAST                     0x1103
#define ICAP_CUSTHALFTONE                 0x1104
#define ICAP_EXPOSURETIME                 0x1105
#define ICAP_FILTER                       0x1106
#define ICAP_FLASHUSED                    0x1107
#define ICAP_GAMMA                        0x1108
#define ICAP_HALFTONES                    0x1109
#define ICAP_HIGHLIGHT                    0x110a
#define ICAP_IMAGEFILEFORMAT              0x110c
#define ICAP_LAMPSTATE                    0x110d
#define ICAP_LIGHTSOURCE                  0x110e
#define ICAP_ORIENTATION                  0x1110
#define ICAP_PHYSICALWIDTH                0x1111
#define ICAP_PHYSICALHEIGHT               0x1112
#define ICAP_SHADOW                       0x1113
#define ICAP_FRAMES                       0x1114
#define ICAP_XNATIVERESOLUTION            0x1116
#define ICAP_YNATIVERESOLUTION            0x1117
#define ICAP_XRESOLUTION                  0x1118
#define ICAP_YRESOLUTION                  0x1119
#define ICAP_MAXFRAMES                    0x111a
#define ICAP_TILES                        0x111b
#define ICAP_BITORDER                     0x111c
#define ICAP_CCITTKFACTOR                 0x111d
#define ICAP_LIGHTPATH                    0x111e
#define ICAP_PIXELFLAVOR                  0x111f
#define ICAP_PLANARCHUNKY                 0x1120
#define ICAP_ROTATION                     0x1121
#define ICAP_SUPPORTEDSIZES               0x1122
#define ICAP_THRESHOLD                    0x1123
#define ICAP_XSCALING                     0x1124
#define ICAP_YSCALING                     0x1125
#define ICAP_BITORDERCODES                0x1126
#define ICAP_PIXELFLAVORCODES             0x1127
#define ICAP_JPEGPIXELTYPE                0x1128
#define ICAP_TIMEFILL                     0x112a
#define ICAP_BITDEPTH                     0x112b
#define ICAP_BITDEPTHREDUCTION            0x112c
#define ICAP_UNDEFINEDIMAGESIZE           0x112d
#define ICAP_IMAGEDATASET                 0x112e
#define ICAP_EXTIMAGEINFO                 0x112f
#define ICAP_MINIMUMHEIGHT                0x1130
#define ICAP_MINIMUMWIDTH                 0x1131
#define ICAP_AUTODISCARDBLANKPAGES        0x1134
#define ICAP_FLIPROTATION                 0x1136
#define ICAP_BARCODEDETECTIONENABLED      0x1137
#define ICAP_SUPPORTEDBARCODETYPES        0x1138
#define ICAP_BARCODEMAXSEARCHPRIORITIES   0x1139
#define ICAP_BARCODESEARCHPRIORITIES      0x113a
#define ICAP_BARCODESEARCHMODE            0x113b
#define ICAP_BARCODEMAXRETRIES            0x113c
#define ICAP_BARCODETIMEOUT               0x113d
#define ICAP_ZOOMFACTOR                   0x113e
#define ICAP_PATCHCODEDETECTIONENABLED    0x113f
#define ICAP_SUPPORTEDPATCHCODETYPES      0x1140
#define ICAP_PATCHCODEMAXSEARCHPRIORITIES 0x1141
#define ICAP_PATCHCODESEARCHPRIORITIES    0x1142
#define ICAP_PATCHCODESEARCHMODE          0x1143
#define ICAP_PATCHCODEMAXRETRIES          0x1144
#define ICAP_PATCHCODETIMEOUT             0x1145
#define ICAP_FLASHUSED2                   0x1146
#define ICAP_IMAGEFILTER                  0x1147
#define ICAP_NOISEFILTER                  0x1148
#define ICAP_OVERSCAN                     0x1149
#define ICAP_AUTOMATICBORDERDETECTION     0x1150
#define ICAP_AUTOMATICDESKEW              0x1151
#define ICAP_AUTOMATICROTATE              0x1152
#define ICAP_JPEGQUALITY                  0x1153
#define ICAP_FEEDERTYPE                   0x1154
#define ICAP_ICCPROFILE                   0x1155
#define ICAP_AUTOSIZE                     0x1156
#define ICAP_AUTOMATICCROPUSESFRAME       0x1157
#define ICAP_AUTOMATICLENGTHDETECTION     0x1158
#define ICAP_AUTOMATICCOLORENABLED        0x1159
#define ICAP_AUTOMATICCOLORNONCOLORPIXELTYPE 0x115a
#define ICAP_COLORMANAGEMENTENABLED       0x115b
#define ICAP_IMAGEMERGE                   0x115c
#define ICAP_IMAGEMERGEHEIGHTTHRESHOLD    0x115d
#define ICAP_SUPPORTEDEXTIMAGEINFO        0x115e
#define ICAP_FILMTYPE                     0x115f
#define ICAP_MIRROR                       0x1160
#define ICAP_JPEGSUBSAMPLING              0x1161

*----- image data sources MAY support these audio caps
#define ACAP_XFERMECH                     0x1202


*----- Extended Image Info Attributes section  Added 1.7 
#define TWEI_BARCODEX               0x1200
#define TWEI_BARCODEY               0x1201
#define TWEI_BARCODETEXT            0x1202
#define TWEI_BARCODETYPE            0x1203
#define TWEI_DESHADETOP             0x1204
#define TWEI_DESHADELEFT            0x1205
#define TWEI_DESHADEHEIGHT          0x1206
#define TWEI_DESHADEWIDTH           0x1207
#define TWEI_DESHADESIZE            0x1208
#define TWEI_SPECKLESREMOVED        0x1209
#define TWEI_HORZLINEXCOORD         0x120A
#define TWEI_HORZLINEYCOORD         0x120B
#define TWEI_HORZLINELENGTH         0x120C
#define TWEI_HORZLINETHICKNESS      0x120D
#define TWEI_VERTLINEXCOORD         0x120E
#define TWEI_VERTLINEYCOORD         0x120F
#define TWEI_VERTLINELENGTH         0x1210
#define TWEI_VERTLINETHICKNESS      0x1211
#define TWEI_PATCHCODE              0x1212
#define TWEI_ENDORSEDTEXT           0x1213
#define TWEI_FORMCONFIDENCE         0x1214
#define TWEI_FORMTEMPLATEMATCH      0x1215
#define TWEI_FORMTEMPLATEPAGEMATCH  0x1216
#define TWEI_FORMHORZDOCOFFSET      0x1217
#define TWEI_FORMVERTDOCOFFSET      0x1218
#define TWEI_BARCODECOUNT           0x1219
#define TWEI_BARCODECONFIDENCE      0x121A
#define TWEI_BARCODEROTATION        0x121B
#define TWEI_BARCODETEXTLENGTH      0x121C
#define TWEI_DESHADECOUNT           0x121D
#define TWEI_DESHADEBLACKCOUNTOLD   0x121E
#define TWEI_DESHADEBLACKCOUNTNEW   0x121F
#define TWEI_DESHADEBLACKRLMIN      0x1220
#define TWEI_DESHADEBLACKRLMAX      0x1221
#define TWEI_DESHADEWHITECOUNTOLD   0x1222
#define TWEI_DESHADEWHITECOUNTNEW   0x1223
#define TWEI_DESHADEWHITERLMIN      0x1224
#define TWEI_DESHADEWHITERLAVE      0x1225
#define TWEI_DESHADEWHITERLMAX      0x1226
#define TWEI_BLACKSPECKLESREMOVED   0x1227
#define TWEI_WHITESPECKLESREMOVED   0x1228
#define TWEI_HORZLINECOUNT          0x1229
#define TWEI_VERTLINECOUNT          0x122A
#define TWEI_DESKEWSTATUS           0x122B
#define TWEI_SKEWORIGINALANGLE      0x122C
#define TWEI_SKEWFINALANGLE         0x122D
#define TWEI_SKEWCONFIDENCE         0x122E
#define TWEI_SKEWWINDOWX1           0x122F
#define TWEI_SKEWWINDOWY1           0x1230
#define TWEI_SKEWWINDOWX2           0x1231
#define TWEI_SKEWWINDOWY2           0x1232
#define TWEI_SKEWWINDOWX3           0x1233
#define TWEI_SKEWWINDOWY3           0x1234
#define TWEI_SKEWWINDOWX4           0x1235
#define TWEI_SKEWWINDOWY4           0x1236
#define TWEI_BOOKNAME               0x1238
#define TWEI_CHAPTERNUMBER          0x1239
#define TWEI_DOCUMENTNUMBER         0x123A
#define TWEI_PAGENUMBER             0x123B
#define TWEI_CAMERA                 0x123C
#define TWEI_FRAMENUMBER            0x123D
#define TWEI_FRAME                  0x123E
#define TWEI_PIXELFLAVOR            0x123F
#define TWEI_ICCPROFILE             0x1240
#define TWEI_LASTSEGMENT            0x1241
#define TWEI_SEGMENTNUMBER          0x1242
#define TWEI_MAGDATA                0x1243
#define TWEI_MAGTYPE                0x1244
#define TWEI_PAGESIDE               0x1245
#define TWEI_FILESYSTEMSOURCE       0x1246
#define TWEI_IMAGEMERGED            0x1247
#define TWEI_MAGDATALENGTH          0x1248
#define TWEI_PAPERCOUNT             0x1249
#define TWEI_PRINTERTEXT            0x124A 

#define TWEJ_NONE                   0x0000
#define TWEJ_MIDSEPARATOR           0x0001
#define TWEJ_PATCH1                 0x0002
#define TWEJ_PATCH2                 0x0003
#define TWEJ_PATCH3                 0x0004
#define TWEJ_PATCH4                 0x0005
#define TWEJ_PATCH6                 0x0006
#define TWEJ_PATCHT                 0x0007

*----- Return Codes and Condition Codes section
#define TWRC_CUSTOMBASE					0x8000
#define TWRC_SUCCESS					0
#define TWRC_FAILURE					1
#define TWRC_CHECKSTATUS				2
#define TWRC_CANCEL						3
#define TWRC_DSEVENT					4
#define TWRC_NOTDSEVENT					5
#define TWRC_XFERDONE					6
#define TWRC_ENDOFLIST					7
#define TWRC_INFONOTSUPPORTED			8
#define TWRC_DATANOTAVAILABLE			9
#define TWRC_BUSY						10
#define TWRC_SCANNERLOCKED				11

*----- Condition Codes: Application gets these by doing DG_CONTROL DAT_STATUS MSG_GET.
#define TWCC_CUSTOMBASE         0x8000

#define TWCC_SUCCESS            0 
#define TWCC_BUMMER             1 
#define TWCC_LOWMEMORY          2 
#define TWCC_NODS               3 
#define TWCC_MAXCONNECTIONS     4 
#define TWCC_OPERATIONERROR     5 
#define TWCC_BADCAP             6 
#define TWCC_BADPROTOCOL        9 
#define TWCC_BADVALUE           10
#define TWCC_SEQERROR           11
#define TWCC_BADDEST            12
#define TWCC_CAPUNSUPPORTED     13
#define TWCC_CAPBADOPERATION    14
#define TWCC_CAPSEQERROR        15
#define TWCC_DENIED             16
#define TWCC_FILEEXISTS         17
#define TWCC_FILENOTFOUND       18
#define TWCC_NOTEMPTY           19
#define TWCC_PAPERJAM           20
#define TWCC_PAPERDOUBLEFEED    21
#define TWCC_FILEWRITEERROR     22
#define TWCC_CHECKDEVICEONLINE  23
#define TWCC_INTERLOCK          24
#define TWCC_DAMAGEDCORNER      25
#define TWCC_FOCUSERROR         26
#define TWCC_DOCTOOLIGHT        27
#define TWCC_DOCTOODARK         28
#define TWCC_NOMEDIA            29

*----- bit patterns: for query the operation that are supported by the data source on a capability
*----- Application gets these through DG_CONTROL/DAT_CAPABILITY/MSG_QUERYSUPPORT
#define TWQC_GET              0x0001 
#define TWQC_SET              0x0002
#define TWQC_GETDEFAULT       0x0004
#define TWQC_GETCURRENT       0x0008
#define TWQC_RESET            0x0010
#define TWQC_SETCONSTRAINT    0x0020
#define TWQC_CONSTRAINABLE    0x0040
#define TWQC_GETHELP          0x0100
#define TWQC_GETLABEL         0x0200
#define TWQC_GETLABELENUM     0x0400

*----- Depreciated Items 
#define TWTY_STR1024          0x000d
#define TWTY_UNI512           0x000e

#define TWFF_JPN              12

#define DAT_TWUNKIDENTITY     0x000b
#define DAT_SETUPFILEXFER2    0x0301

#define CAP_SUPPORTEDCAPSEXT      0x100c
#define CAP_FILESYSTEM            //0x????
#define CAP_PAGEMULTIPLEACQUIRE   0x1023
#define CAP_PAPERBINDING          0x102f
#define CAP_PASSTHRU              0x1031
#define CAP_POWERDOWNTIME         0x1034
#define ACAP_AUDIOFILEFORMAT      0x1201

#define MSG_CHECKSTATUS       0x0201

#define MSG_INVOKE_CALLBACK   0x0903

#define TWSX_FILE2            3

*----- CAP_FILESYSTEM values (FS_ means file system) 
#define TWFS_FILESYSTEM       0
#define TWFS_RECURSIVEDELETE  1

*----- ICAP_PIXELTYPE values (PT_ means Pixel Type) 
#define TWPT_SRGB64     11
#define TWPT_BGR        12
#define TWPT_CIELAB     13
#define TWPT_CIELUV     14
#define TWPT_YCBCR      15

*----- ICAP_SUPPORTEDSIZES values (SS_ means Supported Sizes) 
#define TWSS_B                8
#define TWSS_A4LETTER    TWSS_A4   
#define TWSS_B3          TWSS_ISOB3
#define TWSS_B4          TWSS_ISOB4
#define TWSS_B6          TWSS_ISOB6
#define TWSS_B5LETTER    TWSS_JISB5


*----- ACAP_AUDIOFILEFORMAT values (AF_ means audio format).  Added 1.8 
#define TWAF_WAV      0
#define TWAF_AIFF     1
#define TWAF_AU       3
#define TWAF_SND      4


