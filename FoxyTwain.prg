**************************************************
*-- Class Library:  c:\foxytwain\foxytwain.vcx
*-- Author: Fernando Mora
*-- Location: Machala, Ecuador
**************************************************

**************************************************
*-- Class:        _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  container
*-- BaseClass:    container
*-- Time Stamp:   11/13/19 02:48:13 PM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS _kernel32 AS container


	Width = 20
	Height = 20
	*-- XML Metadata for customizable properties
	_memberdata = [<VFPData><memberdata name="closehandle" type="method" display="CloseHandle"/><memberdata name="createfile" type="method" display="CreateFile"/><memberdata name="getheapalloc" type="method" display="GetHeapAlloc"/><memberdata name="getheapfree" type="method" display="GetHeapFree"/><memberdata name="getstrlena" type="method" display="GetStrLenA"/><memberdata name="getstrlenw" type="method" display="GetStrLenW"/><memberdata name="globallock" type="method" display="GlobalLock"/><memberdata name="globalunlock" type="method" display="GlobalUnLock"/><memberdata name="writefile" type="method" display="WriteFile"/><memberdata name="globalfree" type="method" display="GlobalFree"/><memberdata name="getmessage" type="method" display="GetMessage"/><memberdata name="getheapcreate" type="method" display="GetHeapCreate"/><memberdata name="getheapfreeone" type="method" display="GetHeapFreeOne"/><memberdata name="getprocaddress" type="method" display="GetProcAddress"/><memberdata name="freelibrary" type="method" display="FreeLibrary"/><memberdata name="getmessageerror" type="method" display="GetMessageError"/><memberdata name="getmodulehandlea" type="method" display="GetModuleHandleA"/><memberdata name="loadlibrarya" type="method" display="LoadLibraryA"/><memberdata name="callwindowproca" type="method" display="CallWindowProcA"/><memberdata name="sleep" type="method" display="Sleep"/><memberdata name="globalalloc" type="method" display="GlobalAlloc"/><memberdata name="rtlmovememory" type="method" display="RtlMoveMemory"/><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="heaplock" type="method" display="HeapLock"/><memberdata name="heapunlock" type="method" display="HeapUnLock"/><memberdata name="heapdestroy" type="method" display="HeapDestroy"/><memberdata name="heapsize" type="method" display="HeapSize"/><memberdata name="heapvalidate" type="method" display="HeapValidate"/></VFPData>]
	Name = "_kernel32"


	PROCEDURE createfile
		LPARAMETERS tcFileName AS String
		*----- Declaration API
		DECLARE LONG CreateFileA IN Kernel32 ;
			STRING	 lpFileName,;
			LONG	 dwDesiredAccess,;
			LONG	 dwShareMode,;
			LONG	 lpSecurityAttributes,;
			LONG	 dwCreationDisposition,;
			LONG	 dwFlagsAndAttributes,;
			LONG	 hTemplateFile
		*----- Creamos el archivo
		lpFileName = tcFileName
		dwDesiredAccess = BITOR(GENERIC_READ, GENERIC_WRITE)
		dwShareMode = FILE_SHARE_OFF
		lpSecurityAttributes = 0
		dwCreationDisposition = CREATE_ALWAYS
		dwFlagsAndAttributes = FILE_ATTRIBUTE_ARCHIVE
		hTemplateFile = 0
		hFileCreated = CreateFileA(lpFileName, dwDesiredAccess, dwShareMode, lpSecurityAttributes, ;
									dwCreationDisposition, dwFlagsAndAttributes, hTemplateFile)
		RETURN hFileCreated
	ENDPROC


	PROCEDURE writefile
		LPARAMETERS thFile AS Long, tpBuffer AS Long, tnLenBuffer AS Long
		*----- Declaration API
		DECLARE LONG WriteFile IN Kernel32;
			LONG	 hFile,;
			LONG	 lpBuffer,;
			LONG	 nNumberOfBytesToWrite,;
			LONG	@lpNumberOfBytesWritten,;
			LONG	 lpOverlapped
		*----- Escribimos en archivo
		lpNumberOfBytesWritten = 0
		lpOverlapped = 0
		nResp = WriteFile(thFile, tpBuffer, tnLenBuffer, @lpNumberOfBytesWritten, lpOverlapped)
		IF nResp=0
			THIS.GetMensajeError(GetLastError())
		ENDIF
		CLEAR DLLS WriteFile
		RETURN nResp
	ENDPROC


	PROCEDURE closehandle
		LPARAMETERS thFile AS Long
		*----- Declaration API
		DECLARE LONG CloseHandle IN Kernel32;
			LONG	 hObject
		*----- Cerramos el archivo
		nBool = CloseHandle(thFile)
		CLEAR DLLS CloseHandle, CreateFile
		RETURN nBool
	ENDPROC


	PROCEDURE getheapalloc
		LPARAMETERS tcStrToPint AS String, tbNotClosable_Opt AS Logical
		*----- Declarations APIS
		DECLARE LONG GetProcessHeap IN Kernel32

		DECLARE LONG HeapAlloc IN Kernel32;
			LONG 	hHeap,;
			LONG 	dwFlags,;
			LONG 	dwBytes

		DECLARE RtlMoveMemory IN Kernel32;
			LONG	Destination,;
			STRING @Source,;
			LONG	Length
		*----- Creamos puntero y colocamos datos
		tpAddressMem = HeapAlloc(GetProcessHeap(), 0, LEN(tcStrToPint))
		RtlMoveMemory(tpAddressMem, @tcStrToPint, LEN(tcStrToPint))
		WITH _SCREEN
			IF tbNotClosable_Opt=.F.
				IF VARTYPE(.ArrayPointer)=="U"
					.AddProperty("ArrayPointer(1,2)")
					nLenMat=ALEN(.ArrayPointer,1)
				ELSE
					nLenMat=ALEN(.ArrayPointer,1)+1
					DIMENSION .ArrayPointer(nLenMat,2)
				ENDIF
				.ArrayPointer(nLenMat,1) = tpAddressMem
				.ArrayPointer(nLenMat,2) = LEN(tcStrToPint)
			ENDIF
		ENDWITH
		CLEAR DLLS GetProcessHeap, HeapAlloc, RtlMoveMemory 
		RETURN tpAddressMem
	ENDPROC


	PROCEDURE getheapfree
		*----- Declarations APIS
		DECLARE LONG GetProcessHeap IN Kernel32

		DECLARE LONG HeapReAlloc IN Kernel32;
			LONG	 hHeap,;
			LONG	 dwFlags,;
			LONG	 lpMem,;
			LONG	 dwBytes

		DECLARE LONG IsBadCodePtr IN Kernel32;
			LONG	 lpfn

		DECLARE RtlMoveMemory IN Kernel32;
			LONG	Destination,;
			STRING @Source,;
			LONG	Length

		DECLARE LONG HeapFree IN Kernel32;
			LONG 	hHeap,;
			LONG 	dwFlags,;
			LONG 	lpMem
		*----- Vaciamos, reducimos y liberamos punteros de memoria
		WITH _SCREEN
			IF VARTYPE(.ArrayPointer)#"U"
				FOR H = 1 TO ALEN(.ArrayPointer,1)
					nPointAddress = IIF(VARTYPE(.ArrayPointer(H, 1))=="N", .ArrayPointer(H, 1), 0)
					nLenPointer = IIF(VARTYPE(.ArrayPointer(H, 2))=="N", .ArrayPointer(H, 2), 0)
					*----- Aseguramos que sea un puntero valido y que podamos acceder
					IF IsBadCodePtr(nPointAddress)=0
						TRY
							cReplicate = REPLICATE(" ", nLenPointer)
							RtlMoveMemory(nPointAddress, @cReplicate, nLenPointer)
							IF nLenPointer>0
								HeapReAlloc(GetProcessHeap(), 0x00000004, nPointAddress , 1)
							ENDIF
							nRespFree= HeapFree(GetProcessHeap(), 0, nPointAddress)
						CATCH TO oError
						ENDTRY
					ENDIF
				NEXT
				REMOVEPROPERTY(_SCREEN, "ArrayPointer")
				RELEASE ArrayPointer
			ENDIF
		ENDWITH
		CLEAR DLLS GetProcessHeap, HeapReAlloc, IsBadCodePtr, RtlMoveMemory, HeapFree
	ENDPROC


	PROCEDURE globallock
		LPARAMETERS thPointer AS Long
		*----- Declaration API
		DECLARE LONG GlobalLock IN Kernel32;
			LONG	 hMem
		*----- Bloqueamos el puntero de memoria
		TRY 
			nPtrLock = GlobalLock(thPointer)
		CATCH TO oError
		ENDTRY
		RETURN nPtrLock
	ENDPROC


	PROCEDURE globalunlock
		LPARAMETERS tpPointer AS Lon
		*----- Declaration API
		DECLARE LONG GlobalUnlock IN Kernel32;
			LONG	 hMem
		*----- Desbloqueamos puntero de memoria
		TRY
			nPtrLock = GlobalUnlock(tpPointer)
		CATCH TO oError
		ENDTRY
		CLEAR DLLS GlobalUnlock, GlobalLock
		RETURN nPtrLock
	ENDPROC


	PROCEDURE getstrlena
		LPARAMETERS tpAdderss AS Long
		*----- Declaration API
		DECLARE LONG lstrlenA IN Kernel32 AS GetStrLenA;
			LONG	lpString
		*----- Obtenemos la longitud de puntero ANSI
		nLenAnsi = GetStrLenA(tpAdderss)
		CLEAR DLLS GetStrLenA
		RETURN nLenAnsi
	ENDPROC


	PROCEDURE getstrlenw
		LPARAMETERS tpAdderss AS Long
		*----- Declaration API
		DECLARE LONG lstrlenW IN Kernel32 AS GetStrLenW;
			LONG	lpString
		*----- Obtenemos la longitud de puntero UNICODE
		nLenUnicode = GetStrLenW(tpAdderss)
		CLEAR DLLS GetStrLenw
		RETURN nLenUnicode
	ENDPROC


	PROCEDURE getmessageerror
		LPARAMETERS tnNumError
		*----- Declarations APIS
		DECLARE LONG GetLastError IN Kernel32
		DECLARE LONG FormatMessage IN Kernel32;
		  	LONG 	 dwFlags, ;
		  	STRING 	@lpSource, ;
		  	LONG 	 dwMessageId, ;
		  	LONG 	 dwLanguageId, ;
		  	STRING 	@lpBuffer, ;
		  	LONG 	 nSize, ;
		  	LONG 	 Arguments
		*----- Capturamos el error
		IF VARTYPE(tnNumError)=="N"
			lnErrorCode = tnNumError
		ELSE
			lnErrorCode = GetLastError()
		ENDIF
		lcErrorMessage = SPACE(128)
		=FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, 'WINERROR.H', lnErrorCode, 0, @lcErrorMessage, 128 , 0)
		*!*	WITH THIS
		*!*		.tc_ErrorCode = TRANSFORM(lnErrorCode,"@0")
		*!*		.tc_ErrorMessage = ALLTRIM(lcErrorMessage)
		*!*	ENDWITH
		? lcErrorMessage
		CLEAR DLLS GetLastError, FormatMessage
	ENDPROC


	PROCEDURE globalfree
		LPARAMETERS tpPointer AS Long
		*----- Declaration API
		DECLARE LONG GlobalFree IN Kernel32;
			LONG	 hMen
		*----- Liberamos memoria
		TRY
			nRet = GlobalFree(tpPointer)
		CATCH TO oError
		ENDTRY
		CLEAR DLLS GlobalFree
		RETURN nRet
	ENDPROC


	PROCEDURE getmessage
		LPARAMETERS tpMsg AS Long, tpHWnd AS Long, tnMsgFilterMin AS Long, tnMsgFilterMax AS Long
		*----- Declaration API
		DECLARE LONG GetMessage IN User32;
			LONG	@lpMsg,;
			LONG	 hWnd,;
			LONG	 wMsgFilterMin,;
			LONG	 wMsgFilterMax
		*----- 
		nRetMsg = GetMessage(@tpMsg, tpHWnd, tnMsgFilterMin, tnMsgFilterMax)
		CLEAR DLLS GetMessage
		RETURN nRetMsg
	ENDPROC


	PROCEDURE getheapcreate
		LPARAMETERS tnSize AS Long, tcData AS String
		*----- Declarations API
		DECLARE LONG HeapCreate IN Kernel32;
			LONG	 dwFlags,;
			LONG	 dwBytes,;
			LONG	 dwMaxBytes

		DECLARE LONG HeapAlloc IN Kernel32;
			LONG	 hHeap,;
			LONG	 dwFlags,;
			LONG	 dwBytes

		DECLARE RtlMoveMemory IN Kernel32;
			LONG	 Destination,;
			STRING	@Source,;
			LONG	 Length
		*----- Creamos una pila de memoria
		LOCAL lnProcessAddress
		lnProcessAddress = 0
		hHeap = HeapCreate(HEAP_CREATE_ENABLE_EXECUTE, 0, tnSize)
		hLockHeap = THIS.HeapLock(hHeap)
		lnProcessAddress = HeapAlloc(hHeap, HEAP_GENERATE_EXCEPTIONS, LEN(tcData))
		*----- Cargamos la cadena en el puntero de memoria
		TRY
			RtlMoveMemory(lnProcessAddress, tcData, LEN(tcData))
		CATCH TO oError
			cMensaje = oError.Message
		ENDTRY
		CLEAR DLLS HeapCreate, HeapAlloc, RtlMoveMemory 
		RETURN lnProcessAddress
	ENDPROC


	PROCEDURE getheapfreeone
		LPARAMETERS tpHandle AS Long
		*----- Declarations APIS
		DECLARE LONG GetProcessHeap IN Kernel32

		DECLARE LONG HeapReAlloc IN Kernel32;
			LONG	 hHeap,;
			LONG	 dwFlags,;
			LONG	 lpMem,;
			LONG	 dwBytes

		DECLARE LONG IsBadCodePtr IN Kernel32;
			LONG	 lpfn

		DECLARE RtlMoveMemory IN Kernel32;
			LONG	Destination,;
			STRING @Source,;
			LONG	Length

		DECLARE LONG HeapFree IN Kernel32;
			LONG 	hHeap,;
			LONG 	dwFlags,;
			LONG 	lpMem
		*----- Vaciamos, reducimos y liberamos punteros de memoria
		WITH _SCREEN
			IF VARTYPE(tpHandle)=="N"
				TRY
					HeapFree(GetProcessHeap(), 0, tpHandle)
				CATCH TO oError
				ENDTRY
			ENDIF
		ENDWITH
		CLEAR DLLS GetProcessHeap, HeapReAlloc, IsBadCodePtr, RtlMoveMemory, HeapFree
	ENDPROC


	PROCEDURE getprocaddress
		LPARAMETERS thModle AS Long, tcModulo AS String
		*----- Declarations API
		DECLARE LONG GetProcAddress IN Kernel32;
			LONG	 hModule,;
			STRING	 procname
		*----- Creamos dirección de proceso
		nProccAddress = GetProcAddress(thModle, tcModulo)
		CLEAR DLLS GetProcAddress 
		RETURN nProccAddress 
	ENDPROC


	PROCEDURE freelibrary
		LPARAMETERS thLibrary AS Long
		*----- Declaration API
		DECLARE LONG FreeLibrary IN Kernel32;
			LONG	 hLibModule
		*----- Liberamos libreria
		iBool = 0
		iBool = FreeLibrary(thLibrary)
		CLEAR DLLS FreeLibrary
		RETURN iBool 
	ENDPROC


	PROCEDURE loadlibrarya
		LPARAMETERS tcLibraryName AS String
		*----- Declaration API
		DECLARE LONG LoadLibraryA IN Kernel32;
			STRING	 lpLibFileName
		*----- Cargamos Libreria
		hLibrary = 0
		hLibrary = LoadLibraryA(tcLibraryName)
		CLEAR DLLS LoadLibraryA
		RETURN hLibrary
	ENDPROC


	PROCEDURE getmodulehandlea
		LPARAMETERS tcModuleName AS String
		*----- Declaration API
		DECLARE LONG GetModuleHandleA IN Kernel32;
			STRING	 lpModuleName
		*----- Cargamos Modulo
		lhModule = 0
		lhModule = GetModuleHandleA(tcModuleName)
		CLEAR DLLS GetModuleHandleA
		RETURN lhModule
	ENDPROC


	PROCEDURE callwindowproca
		LPARAMETERS tpPrevWndFunc AS Long, tchWnd AS String, tnMsg AS Long, tnwParam AS Long, tplParam AS Long
		*----- Declaration API
		*!*	DECLARE LONG CallWindowProc IN User32;
		*!*		LONG 	 lpPrevWndFunc,;
		*!*		STRING	@hWnd,;
		*!*		LONG	 Msg,;
		*!*		LONG	 wParam,;
		*!*		LONG	 lParam
		DECLARE LONG CallWindowProc IN User32;
			LONG 	 lpPrevWndFunc,;
			LONG	 hWnd,;
			STRING	 Msg,;
			LONG	 wParam,;
			LONG	 lParam
		*----- Llamamos a un proceso
		nResp = 0
		*nResp = CallWindowProc(tpPrevWndFunc, @tchWnd, tnMsg, tnwParam, tplParam)
		nResp = CallWindowProc(tpPrevWndFunc, tchWnd, tnMsg, tnwParam, tplParam)
		CLEAR DLLS CallWindowProcA
		RETURN nResp
	ENDPROC


	PROCEDURE sleep
		LPARAMETERS tnTime_millisec AS Long
		*----- Declaration API
		DECLARE LONG Sleep IN Kernel32;
			LONG	 dwMilliseconds
		*----- Hacemos una pausa
		lnTimeSleep = IIF(VARTYPE(tnTime_millisec)=="N", tnTime_millisec, 500)
		nBool = Sleep(lnTimeSleep)
		CLEAR DLLS Sleep
		RETURN nBool
	ENDPROC


	PROCEDURE globalalloc
		LPARAMETERS tnSize AS Long
		*----- Declaration API
		DECLARE LONG GlobalAlloc IN Kernel32;
			LONG	 uFlags,;
			LONG	 dwBytes
		*----- Creamos un puntero de memoria global
		lhPointerGlobal = GlobalAlloc(0x0040, tnSize)
		CLEAR DLLS GlobalAlloc
		RETURN lhPointerGlobal
	ENDPROC


	PROCEDURE rtlmovememory
		LPARAMETERS tpAddressMem AS Long, tcStrToPtr AS String
		*----- Declaration API
		DECLARE RtlMoveMemory IN Kernel32;
			LONG	Destination,;
			STRING @Source,;
			LONG	Length
		*----- Movemos Cadena a Puntero de Memoria
		lRet = .F.
		TRY
			RtlMoveMemory(tpAddressMem, @tcStrToPtr, LEN(tcStrToPtr))
			lRet = .T.
		CATCH TO oError
		ENDTRY
		CLEAR DLLS RtlMoveMemory
		RETURN lRet
	ENDPROC


	PROCEDURE get
		nR=AMEMBERS(fer, THIS)
		FOR XO = 1 TO ALEN(fer)
			DO CASE
			CASE LEFT(fer(XO),2)="tn"

			ENDCASE
			*? fer(XO)
		NEXT
	ENDPROC


	PROCEDURE heaplock
		LPARAMETERS tnHeapToLock AS Long
		*----- Declaration API
		DECLARE LONG HeapLock IN Kernel32;
			LONG	 hHeap
		*----- Bloqueamos una pila
		nRespLock = HeapLock(tnHeapToLock)
		CLEAR DLLS HeapLock 
		RETURN nRespLock
	ENDPROC


	PROCEDURE heapunlock
		LPARAMETERS thHeapToUnLock AS Long
		*----- Declaration API
		DECLARE LONG HeapUnlock IN Kernel32;
			LONG	 hHeap
		*----- Desbloqueamos una pila
		nRespUnLock = HeapUnlock(thHeapToUnLock)
		CLEAR DLLS HeapUnlock
		RETURN nRespUnLock
	ENDPROC


	PROCEDURE heapsize
		LPARAMETERS tnHeapRecoverSize AS Long
		*----- Declaration API
		DECLARE LONG HeapSize IN Kenerel 32;
			LONG	 hHeap,;
			LONG	 dwFlags,;
			LONG	 lpMem
		*----- Recuperamos el tamaño de una pila de memoria
		nHeapSize = HeapSize(tnHeapRecoverSize, 1, 0)
		CLEAR DLLS HeapSize
		RETURN nHeapSize
	ENDPROC


	PROCEDURE heapdestroy
		LPARAMETERS tnHeapToDestroy AS Long
		*----- Declaration API
		DECLARE LONG HeapDestroy IN Kernel32;
			LONG	 hHeap
		*----- Destruimos un pila de memoria
		nRespDestroy = 0
		TRY
			nRespDestroy = HeapDestroy(tnHeapToDestroy)
		CATCH TO oErro
		ENDTRY
		CLEAR DLLS HeapDestroy
		RETURN nRespDestroy
	ENDPROC


	PROCEDURE heapvalidate
		LPARAMETERS tnHeapToValidate AS Long
		*----- Declaration API
		DECLARE LONG HeapValidate IN Kernel32;
			LONG	 hHeap,;
			LONG	 dwFlags,;
			LONG	 lpMem
		*----- Validamos una pila de memoria
		nRespVali = HeapValidate(tnHeapToValidate, 1, 0)
		CLEAR DLLS HeapValidate 
		RETURN nRespVali 
	ENDPROC


	PROCEDURE set
	ENDPROC


ENDDEFINE
*
*-- EndDefine: _kernel32
**************************************************


**************************************************
*-- Class:        bmp_header (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 08:38:00 PM
*
DEFINE CLASS bmp_header AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_ftype = 0
	*-- 4RS, 32bits, Long
	tn_fsize = 0
	*-- 2RS, 16bits, Short
	tn_freserved1 = 0
	*-- 2RS, 16bits, Short
	tn_freserved2 = 0
	*-- 4RS, 32bits, Long
	tn_foffbits = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_foffbits" type="property" display="tLong_FoffBits"/><memberdata name="tlong_fsize" type="property" display="tLong_Fsize"/><memberdata name="tshort_freserved1" type="property" display="tShort_Freserved1"/><memberdata name="tshort_freserved2" type="property" display="tShort_Freserved2"/><memberdata name="tshort_ftype" type="property" display="tShort_Ftype"/><memberdata name="tn_foffbits" type="property" display="tn_FOffBits"/><memberdata name="tn_freserved1" type="property" display="tn_FReserved1"/><memberdata name="tn_freserved2" type="property" display="tn_FReserved2"/><memberdata name="tn_fsize" type="property" display="tn_FSize"/><memberdata name="tn_ftype" type="property" display="tn_FType"/></VFPData>]
	Name = "bmp_header"


	PROCEDURE get
		BITMAPFILEHEADER = ""
		WITH THIS
		    bfType		= BINTOC(IIF(VARTYPE(.tn_FType )="N", .tn_FType , 0), "2RS")
		    bfSize		= BINTOC(IIF(VARTYPE(.tn_FSize)="N", .tn_FSize, 0), "4RS")
		    bfReserved1	= BINTOC(IIF(VARTYPE(.tn_FReserved1)="N", .tn_FReserved1, 0), "2RS")
		    bfReserved2	= BINTOC(IIF(VARTYPE(.tn_FReserved2)="N", .tn_FReserved2, 0), "2RS")
		    bfOffBits	= BINTOC(IIF(VARTYPE(.tn_FOffBits)="N", .tn_FOffBits, 0), "4RS")
		    BITMAPFILEHEADER = bfType + bfSize + bfReserved1 + bfReserved2 + bfOffBits
		ENDWITH
		RETURN BITMAPFILEHEADER
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_BMP_HEADER AS String
		lBool=.F.
		IF VARTYPE(tcStruct_BMP_HEADER)=="C" OR VARTYPE(tcStruct_BMP_HEADER)=="Q"
			WITH THIS
				.tn_FType 		= CTOBIN(SUBSTR(tcStruct_BMP_HEADER,1,2), "2RS")
				.tn_FSize		= CTOBIN(SUBSTR(tcStruct_BMP_HEADER,3,4), "4RS")
				.tn_FReserved1	= CTOBIN(SUBSTR(tcStruct_BMP_HEADER,7,2), "2RS")
				.tn_FReserved2	= CTOBIN(SUBSTR(tcStruct_BMP_HEADER,9,2), "2RS")
				.tn_FOffBits	= CTOBIN(SUBSTR(tcStruct_BMP_HEADER,11,4), "4RS")
			ENDWITH
			lBool=.T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: bmp_header
**************************************************


**************************************************
*-- Class:        bmp_info (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 08:49:01 PM
*
DEFINE CLASS bmp_info AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 4RS, 32bits, Long
	tn_size = 0
	*-- 4RS, 32bits, Long
	tn_width = 0
	*-- 4RS, 32bits, Long
	tn_height = 0
	*-- 2RS, 16bits, Short
	tn_planes = 0
	*-- 2RS, 16bits, Short
	tn_bitcount = 0
	*-- 4RS, 32bits, Long
	tn_compression = 0
	*-- 4RS, 32bits, Long
	tn_sizeimage = 0
	*-- 4RS, 32bits, Long
	tn_xpelspermeter = 0
	*-- 4RS, 32bits, Long
	tn_ypelspermeter = 0
	*-- 4RS, 32bits, Long
	tn_clrused = 0
	*-- 4RS, 32bits, Long
	tn_clrimportant = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_clrimportant" type="property" display="tLong_ClrImportant"/><memberdata name="tlong_clrused" type="property" display="tLong_ClrUsed"/><memberdata name="tlong_compression" type="property" display="tLong_Compression"/><memberdata name="tlong_height" type="property" display="tLong_Height"/><memberdata name="tlong_size" type="property" display="tLong_Size"/><memberdata name="tlong_sizeimage" type="property" display="tLong_SizeImage"/><memberdata name="tlong_width" type="property" display="tLong_Width"/><memberdata name="tlong_xpelspermeter" type="property" display="tLong_XPelsPerMeter"/><memberdata name="tlong_ypelspermeter" type="property" display="tLong_YPelsPerMeter"/><memberdata name="tshort_bitcount" type="property" display="tShort_BitCount"/><memberdata name="tshort_planes" type="property" display="tShort_Planes"/><memberdata name="tn_bitcount" type="property" display="tn_BitCount"/><memberdata name="tn_clrimportant" type="property" display="tn_ClrImportant"/><memberdata name="tn_clrused" type="property" display="tn_ClrUsed"/><memberdata name="tn_compression" type="property" display="tn_Compression"/><memberdata name="tn_height" type="property" display="tn_Height"/><memberdata name="tn_planes" type="property" display="tn_Planes"/><memberdata name="tn_size" type="property" display="tn_Size"/><memberdata name="tn_sizeimage" type="property" display="tn_SizeImage"/><memberdata name="tn_width" type="property" display="tn_Width"/><memberdata name="tn_xpelspermeter" type="property" display="tn_XPelsPerMeter"/><memberdata name="tn_ypelspermeter" type="property" display="tn_YPelsPerMeter"/></VFPData>]
	Name = "bmp_info"


	PROCEDURE get
		BITMAPINFOHEADER = ""
		WITH THIS
		    biSize			= BINTOC(IIF(VARTYPE(.tn_Size)="N", .tn_Size, 0), "4RS")
		    biWidth			= BINTOC(IIF(VARTYPE(.tn_Width)="N", .tn_Width, 0), "4RS")
		    biHeight		= BINTOC(IIF(VARTYPE(.tn_Height)="N", .tn_Height, 0), "4RS")
		    biPlanes		= BINTOC(IIF(VARTYPE(.tn_Planes)="N", .tn_Planes, 0), "2RS")
		    biBitCount		= BINTOC(IIF(VARTYPE(.tn_BitCount)="N", .tn_BitCount, 0), "2RS")
		    biCompression	= BINTOC(IIF(VARTYPE(.tn_Compression)="N", .tn_Compression, 0), "4RS")
		    biSizeImage		= BINTOC(IIF(VARTYPE(.tn_SizeImage)="N", .tn_SizeImage, 0), "4RS")
		    biXPelsPerMeter	= BINTOC(IIF(VARTYPE(.tn_XPelsPerMeter)="N", .tn_XPelsPerMeter, 0), "4RS")
		    biYPelsPerMeter	= BINTOC(IIF(VARTYPE(.tn_YPelsPerMeter)="N", .tn_YPelsPerMeter, 0), "4RS")
		    biClrUsed		= BINTOC(IIF(VARTYPE(.tn_ClrUsed)="N", .tn_ClrUsed, 0), "4RS")
		    biClrImportant	= BINTOC(IIF(VARTYPE(.tn_ClrImportant)="N", .tn_ClrImportant, 0), "4RS")
		    BITMAPINFOHEADER = biSize + biWidth + biHeight + biPlanes + biBitCount + biCompression + ;
		    					biSizeImage + biXPelsPerMeter + biYPelsPerMeter + biClrUsed + biClrImportant
		ENDWITH
		RETURN BITMAPINFOHEADER
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_BMP_INFO AS String
		lBool=.F.
		IF VARTYPE(tcStruct_BMP_INFO)=="C" OR VARTYPE(tcStruct_BMP_INFO)=="Q"
			WITH THIS
				.tn_Size			= CTOBIN(SUBSTR(tcStruct_BMP_INFO, 1, 4), "4RS")
				.tn_Width			= CTOBIN(SUBSTR(tcStruct_BMP_INFO, 5, 4), "4RS")
				.tn_Height			= CTOBIN(SUBSTR(tcStruct_BMP_INFO, 9, 4), "4RS")
				.tn_Planes			= CTOBIN(SUBSTR(tcStruct_BMP_INFO,13, 2), "2RS")
				.tn_BitCount		= CTOBIN(SUBSTR(tcStruct_BMP_INFO,15, 2), "2RS")
				.tn_Compression		= CTOBIN(SUBSTR(tcStruct_BMP_INFO,17, 4), "4RS")
				.tn_SizeImage		= CTOBIN(SUBSTR(tcStruct_BMP_INFO,21, 4), "4RS")
				.tn_XPelsPerMeter	= CTOBIN(SUBSTR(tcStruct_BMP_INFO,25, 4), "4RS")
				.tn_YPelsPerMeter	= CTOBIN(SUBSTR(tcStruct_BMP_INFO,29, 4), "4RS")
				.tn_ClrUsed			= CTOBIN(SUBSTR(tcStruct_BMP_INFO,33, 4), "4RS")
				.tn_ClrImportant	= CTOBIN(SUBSTR(tcStruct_BMP_INFO,37, 4), "4RS")
			ENDWITH
			lBool=.T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: bmp_info
**************************************************


**************************************************
*-- Class:        foxytwain (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/13/19 02:52:00 PM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS foxytwain AS _kernel32


	Width = 262
	Height = 57
	Visible = .F.
	tp_appid = 0
	tp_sourceid = 0
	tn_total = 0
	tn_showui = 0
	tn_modalui = 0
	tc_folderout = "C:\FOXYTWAIN\"
	tp_proccaddress = 0
	th_module = 0
	_memberdata = [<VFPData><memberdata name="closetwaindsm" type="method" display="CloseTwainDSM"/><memberdata name="opentwaindsm" type="method" display="OpenTwainDSM"/><memberdata name="dsm_entry" type="method" display="DSM_Entry"/><memberdata name="selectsource" type="method" display="SelectSource"/><memberdata name="tc_data" type="property" display="tc_Data"/><memberdata name="tp_appid" type="property" display="tp_Appid"/><memberdata name="closetwainds" type="method" display="CloseTwainDS"/><memberdata name="opentwainds" type="method" display="OpenTwainDS"/><memberdata name="getallsource" type="method" display="GetAllSource"/><memberdata name="getcurrentsource" type="method" display="GetCurrentSource"/><memberdata name="getdefaultsource" type="method" display="GetDefaultSource"/><memberdata name="getimagensave" type="method" display="GetImagenSave"/><memberdata name="gettranferimagen" type="method" display="GetTranferImagen"/><memberdata name="tp_sourceid" type="property" display="tp_SourceID"/><memberdata name="addresource" type="method" display="AddResource"/><memberdata name="removeresource" type="method" display="RemoveResource"/><memberdata name="tn_total" type="property" display="tn_Total"/><memberdata name="sources" type="property" display="Sources"/><memberdata name="getcap" type="method" display="GetCap"/><memberdata name="setcap" type="method" display="SetCap"/><memberdata name="tn_modalui" type="property" display="tn_ModalUI"/><memberdata name="tn_showui" type="property" display="tn_ShowUI"/><memberdata name="iscapsupported" type="method" display="IsCapSupported"/><memberdata name="getcurrentcap" type="method" display="GetCurrentCap"/><memberdata name="getdefaultcap" type="method" display="GetDefaultCap"/><memberdata name="getcollectioncap" type="method" display="GetCollectionCap"/><memberdata name="gettypecap" type="method" display="GetTypeCap"/><memberdata name="tc_folderout" type="property" display="tc_FolderOut"/><memberdata name="getelementcap" type="method" display="GetElementCap"/><memberdata name="getnamecap" type="method" display="GetNameCap"/><memberdata name="statustwaindsm" type="method" display="StatusTwainDSM"/><memberdata name="th_module" type="property" display="th_Module"/><memberdata name="tp_proccaddress" type="property" display="tp_ProccAddress"/><memberdata name="getentrypoint" type="method" display="GetEntryPoint"/></VFPData>]
	Name = "foxytwain"


	ADD OBJECT struct1 AS struct WITH ;
		Top = 3, ;
		Left = 31, ;
		ZOrderSet = 0, ;
		Name = "Struct1", ;
		Tw_identity1.Tw_version1.Name = "Tw_version1", ;
		Tw_identity1.Name = "Tw_identity1", ;
		Tw_userinterface1.Name = "Tw_userinterface1", ;
		Tw_msg1.Name = "Tw_msg1", ;
		Tw_event1.Name = "Tw_event1", ;
		Tw_pending_xfers1.Name = "Tw_pending_xfers1", ;
		Bmp_info1.Name = "Bmp_info1", ;
		Bmp_header1.Name = "Bmp_header1", ;
		Tw_capability1.Name = "Tw_capability1", ;
		Tw_onevalue1.Tw_fix321.Name = "Tw_fix321", ;
		Tw_onevalue1.Name = "Tw_onevalue1", ;
		Tw_fix321.Name = "Tw_fix321", ;
		Tw_range1.Tw_fix321.Name = "Tw_fix321", ;
		Tw_range1.Name = "Tw_range1", ;
		Tw_enumeration1.Tw_fix321.Name = "Tw_fix321", ;
		Tw_enumeration1.Name = "Tw_enumeration1", ;
		Tw_array1.Tw_fix321.Name = "Tw_fix321", ;
		Tw_array1.Name = "Tw_array1", ;
		Tw_imageinfo1.Tw_fix32XResol.Name = "Tw_fix32XResol", ;
		Tw_imageinfo1.Tw_fix32YResol.Name = "Tw_fix32YResol", ;
		Tw_imageinfo1.Name = "Tw_imageinfo1", ;
		Tw_imagelayout1.Tw_frame1.Tw_fix32Left.Name = "Tw_fix32Left", ;
		Tw_imagelayout1.Tw_frame1.Tw_fix32Right.Name = "Tw_fix32Right", ;
		Tw_imagelayout1.Tw_frame1.Tw_fix32Top.Name = "Tw_fix32Top", ;
		Tw_imagelayout1.Tw_frame1.Tw_fix32Bottom.Name = "Tw_fix32Bottom", ;
		Tw_imagelayout1.Tw_frame1.Name = "Tw_frame1", ;
		Tw_imagelayout1.Name = "Tw_imagelayout1", ;
		Tw_frame1.Tw_fix32Left.Name = "Tw_fix32Left", ;
		Tw_frame1.Tw_fix32Right.Name = "Tw_fix32Right", ;
		Tw_frame1.Tw_fix32Top.Name = "Tw_fix32Top", ;
		Tw_frame1.Tw_fix32Bottom.Name = "Tw_fix32Bottom", ;
		Tw_frame1.Name = "Tw_frame1"


	ADD OBJECT twaindsm1 AS twaindsm WITH ;
		Top = 10, ;
		Left = 11, ;
		ZOrderSet = 1, ;
		Name = "Twaindsm1"


	ADD OBJECT capabilitys1 AS capabilitys WITH ;
		Top = 30, ;
		Left = 5, ;
		Name = "Capabilitys1"


	ADD OBJECT imgfile1 AS imgfile WITH ;
		Top = 30, ;
		Left = 233, ;
		Name = "Imgfile1"


	PROCEDURE opentwaindsm
		nResp = 1
		WITH THIS
			lcMiAppi = .Struct1.Tw_Identity1.Get()
			nGlobAllo = .GlobalAlloc(LEN(lcMiAppi))
			nRespRtl = .RtlMoveMemory(nGlobAllo, lcMiAppi)
			.tp_AppID = .GlobalLock(nGlobAllo)
			IF .tp_AppID=0
				.GetMessageError()
				RETURN 
			ENDIF
			*.tp_AppID = .GetHeapCreate(4096, .Struct1.Tw_Identity1.Get())
			*lBool = .HeapLock(.tp_AppID)
			*.tp_AppID = .GlobalLock(.GetHeapAlloc(.Struct1.Tw_Identity1.Get(), .T.))
			lpZero = 0
			lcHWnd = BINTOC(_SCREEN.HWnd,"4RS")
			nResp = 1
			nResp = .TwainDSM1.DSM_Entry(.tp_AppID, lpZero, DG_CONTROL, DAT_PARENT, MSG_OPENDSM, lcHWnd)
			IF nResp#0
				.GetMessageError(nResp)
			ENDIF
		ENDWITH
		RETURN nResp
	ENDPROC


	PROCEDURE closetwaindsm
		*------ Cerramos el administrador de origenes
		nRes = 1
		WITH THIS
			lpZero = 0
			lcHWnd = BINTOC(_SCREEN.HWnd,"4RS")
			nRes = 1
			nRes = .TwainDSM1.DSM_Entry(.tp_AppID, lpZero, DG_CONTROL, DAT_PARENT, MSG_CLOSEDSM, lcHWnd)
			nUnLock = .HeapUnLock(.tp_AppID)
			lBool = .GetHeapFreeOne(.tp_AppID)
			lBool = .HeapDestroy(.tp_AppID)
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE selectsource
		WITH THIS
			tpZero = 0
			lpSrcID = SPACE(160)
			nRes = 1
			nRes = .TwainDSM1.DSM_Entry(.tp_Appid, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_USERSELECT, lpSrcID)
			IF nRes#0
				.GetMessageError(nRes)
			ELSE 
				lpSrcID = .TwainDSM1.tc_Data
				.tp_SourceID = .GetHeapAlloc(lpSrcID, .T.)
			ENDIF
		ENDWITH
		RETURN lpSrcID
	ENDPROC


	PROCEDURE opentwainds
		WITH THIS
			tpZero = 0
			nRes = 0
			lc_SourceID = ALLTRIM(.TwainDSM1.tc_Data)
			nRes = .TwainDSM1.DSM_Entry(.tp_Appid, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_OPENDS, lc_SourceID)
			IF nRes==0
				.tp_SourceID = .GlobalLock(.GetHeapAlloc(ALLTRIM(.TwainDSM1.tc_Data),.T.))
			ENDIF
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE closetwainds
		*------ Cerramos un dispositivo de origen
		WITH THIS
			tpZero = 0
			nRes = 0
			lc_SourceID = ALLTRIM(.TwainDSM1.tc_Data)
			nRes = .TwainDSM1.DSM_Entry(.tp_AppID, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_CLOSEDS, lc_SourceID)
			IF nRes==0
				rBool = .GlobalUnLock(.tp_SourceID)
				rBool = .GlobalFree(.tp_SourceID)
				.TwainDSM1.tc_Data = ""
			ENDIF
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE getdefaultsource
		WITH THIS
			lpSrcID = SPACE(160)
			tpZero = 0
			nRes = 1
			nRes = .TwainDSM1.DSM_Entry(.tp_AppID, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_GETDEFAULT, lpSrcID)
			IF nRes#0
				.GetMessageError()
			ELSE
				lpSrcID = .TwainDSM1.tc_Data
				.tp_SourceID = .GlobalLock(.GetHeapAlloc(lpSrcID, .T.))
			ENDIF
		ENDWITH
		RETURN lpSrcID
	ENDPROC


	PROCEDURE getallsource
		*------ Recupera tosos las fuentes de origenes instaladas (Escanners, cámaras, etc.)
		*------ Los dispositivos encontrados se colocaran en un objeto colección "Soucres"
		WITH THIS
			lpSrcID = SPACE(160)
			tpZero = 0
			nRes = 1
			nRes = .TwainDSM1.DSM_Entry(.tp_AppID, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_GETFIRST, lpSrcID)
			IF nRes==0
				IF VARTYPE(.Sources)=="O"
					.RemoveObject("Sources")
					.tn_Total=0
				ENDIF
				Sources = CREATEOBJECT("COLLECTION")
				.AddObject("Sources", "COLLECTION")
			ENDIF
			DO WHILE nRes=0
				.AddResource()
				lpSrcID = SPACE(160)
				nRes = .TwainDSM1.DSM_Entry(.tp_AppID, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_GETNEXT, lpSrcID)
			ENDDO
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE getcurrentsource
		WITH THIS
			lpSrcID = SPACE(160)
			tpZero = 0
			nRes = 0
			nRes = .TwainDSM1.DSM_Entry(.tp_AppID, tpZero, DG_CONTROL, DAT_IDENTITY, MSG_GETCURRENT, lpSrcID)
			IF nRes#0
				.GetMessageError(nRes)
			ELSE
				lpSrcID = .TwainDSM1.tc_Data
			ENDIF
		ENDWITH
		RETURN lpSrcID
	ENDPROC


	PROCEDURE gettranferimagen
		WITH THIS
			DOEVENTS
			bReturn = .F.
			tcPending = .Struct1.Tw_Pending_XFers1.Get()
			.Struct1.Tw_UserInterface1.tn_ShowUI = .tn_ShowUI
			.Struct1.Tw_UserInterface1.tn_ModalUI = .tn_ModalUI
			tUI = .Struct1.Tw_UserInterface1.Get()
			nResA = 1
			*------ Habilitamos el dispositivo de origen
			nResA = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_USERINTERFACE, MSG_ENABLEDS, tUI)
			IF nResA=0
				cMSG = .Struct1.Tw_Msg1.Get()
				pMSG = .GetHeapAlloc(cMSG)
				.Struct1.Tw_Event1.tn_Event = pMSG
				tcEvent = .Struct1.Tw_Event1.Get()
				nCont = 0
				DO WHILE .GetMessage(pMSG, 0, 0, 0)#0
					nCont = nCont + 1
					nResB = 1
					*----- Procesamos los eventos del dispostivo de origen
					nResB = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_EVENT, MSG_PROCESSEVENT, tcEvent)
					.Struct1.Tw_Event1.Set(.TwainDSM1.tc_Data)
					DO CASE
					CASE .Struct1.Tw_Event1.tn_TwMessage=MSG_XFERREADY
						*----- Si esta listo para trasmitir imagen, recuperamos la imagen
						lhDIB = BINTOC(0, "4RS")
						nRespC = 1
						nRespC = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_IMAGE, DAT_IMAGENATIVEXFER, MSG_GET, @lhDIB)
						hFileScan = CTOBIN(lhDIB, "4RS")
						IF hFileScan>0
							*------ Si ya hay una imagen, la grabamos a archivo (solo Mapa de Bits por ahora)
							bReturn = .GetImagenSave(hFileScan)
							*------ Averiaguamos si hay alguna imagen mas pendiente por transmitir, si no hay pendietes finalizamos
							nRespD = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_PENDINGXFERS, MSG_ENDXFER, tcPending)
							.Struct1.Tw_Pending_XFers1.Set(.TwainDSM1.tc_Data)
							IF .Struct1.Tw_Pending_XFers1.tn_Count=0
								EXIT
							ENDIF
						ENDIF
					CASE .Struct1.Tw_Event1.tn_TwMessage=MSG_CLOSEDSREQ
						*------ El dispotivo termino de transmitir
						EXIT
					CASE .Struct1.Tw_Event1.tn_TwMessage=MSG_CLOSEDSOK
						*------ El dispotivo pide cerrar la fuente de origen
						EXIT
					CASE nResB=5 AND nCont>20
						*------ Esto lo puse para que el usuario pueda usar la interface si la necesita.
						iBool = .Sleep(1000)
					ENDCASE
				ENDDO
			ENDIF
			*----- Deshabilitamos el dispositivo de origen
			nRespE = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_USERINTERFACE, MSG_DISABLEDS, tUI)
		ENDWITH
		RETURN bReturn
	ENDPROC


	PROCEDURE getimagensave
		LPARAMETERS thFileScan AS Long
		lBool=.F.
		WITH THIS
			cRandon = SYS(2015)
			tcFileName = ADDBS(.tc_FolderOut) + "Fox" + cRandon + ".BMP"
			lpInfoHeader = CTOBIN(SYS(2600, thFileScan, 4), "4RS")
			lcInfoHeader = SYS(2600, lpInfoHeader, 40)
			.Struct1.Bmp_Info1.Set(lcInfoHeader)
			.Struct1.Bmp_Header1.tn_Ftype = 19778
			.Struct1.Bmp_Header1.tn_FoffBits = 14 + .Struct1.Bmp_Info1.tn_Size + (.Struct1.Bmp_Info1.tn_ClrUsed*4)
			.Struct1.Bmp_Header1.tn_FSize = .Struct1.Bmp_Info1.tn_SizeImage + .Struct1.Bmp_Header1.tn_FOffBits
			lcFileHeader = .Struct1.Bmp_Header1.Get()
			lpBmpHeader = .GetHeapAlloc(lcFileHeader)
			nPtrLock = .GlobalLock(thFileScan)
			hFileBMP  = .CreateFile(tcFileName)
			IF hFileBMP>0
				nWrited = .WriteFile(hFileBMP, lpBmpHeader, 14)
				nWrited = .WriteFile(hFileBmp, nPtrLock, (.Struct1.Bmp_Info1.tn_Size+(.Struct1.Bmp_Info1.tn_ClrUsed*4)+.Struct1.Bmp_Info1.tn_SizeImage))
				nClosed = .CloseHandle(hFileBmp)
				lBool=.T.
			ENDIF
			nBool = .GlobalUnLock(nPtrLock)
			*nBool = .GlobalFree(nPtrLock)
			nBool = .GlobalFree(thFileScan)
			.ImgFile1.Add(tcFileName, tcFileName)
		ENDWITH
		RETURN lBool
	ENDPROC


	PROCEDURE addresource
		WITH THIS
			LOCAL loSource, lbinSource
			lbinSource = .TwainDSM1.tc_Data
			.Struct1.Tw_Identity1.Set(lbinSource)
			.tn_Total = .tn_Total + 1
			lcName = "Source" + TRANSFORM(.tn_Total)
			loSource = CREATEOBJECT("EMPTY")
			ADDPROPERTY(loSource, "Name", lcName)
			ADDPROPERTY(loSource, "Id", .Struct1.Tw_Identity1.tn_Id)
			ADDPROPERTY(loSource, "MajorNum", .Struct1.Tw_Identity1.Tw_Version1.tn_MajorNum)
			ADDPROPERTY(loSource, "MinorNum", .Struct1.Tw_Identity1.Tw_Version1.tn_MinorNum)
			ADDPROPERTY(loSource, "Country", .Struct1.Tw_Identity1.Tw_Version1.tn_Country)
			ADDPROPERTY(loSource, "Language", .Struct1.Tw_Identity1.Tw_Version1.tn_Language)
			ADDPROPERTY(loSource, "Info", STRTRAN(.Struct1.Tw_Identity1.Tw_Version1.tc_Info, CHR(0), ""))
			ADDPROPERTY(loSource, "ProtocolMajor", .Struct1.Tw_Identity1.tn_ProtocolMajor)
			ADDPROPERTY(loSource, "ProtocolMinor", .Struct1.Tw_Identity1.tn_ProtocolMinor)
			ADDPROPERTY(loSource, "SupportedGroup", .Struct1.Tw_Identity1.tn_SupportedGroup)
			ADDPROPERTY(loSource, "Manufacturer", STRTRAN(.Struct1.Tw_Identity1.tc_Manufacturer, CHR(0), ""))
			ADDPROPERTY(loSource, "ProductFamily", STRTRAN(.Struct1.Tw_Identity1.tc_ProductFamily, CHR(0), ""))
			ADDPROPERTY(loSource, "ProductName", STRTRAN(.Struct1.Tw_Identity1.tc_ProductName, CHR(0), ""))
			ADDPROPERTY(loSource, "BinarySource", lbinSource)
			.Sources.Add(loSource)
			RELEASE loSource
		ENDWITH
	ENDPROC


	PROCEDURE getcap
		LPARAMETERS tnType AS Long
		IF VARTYPE(tnType)#"N"
			MESSAGEBOX("Codigo de capacidad no valido", 16, _SCREEN.Caption)
			RETURN
		ENDIF
		*------ Recuperamos los valores una capacidad
		loCapRet=NULL
		WITH THIS
				*----- Estrcutura TW_CAPABILITY
				.Struct1.Tw_Capability1.tn_Cap = tnType
				.Struct1.Tw_Capability1.tn_Contype = TWON_DONTCARE8
				.Struct1.Tw_Capability1.tn_HContainer = 0
				tCapability = .Struct1.Tw_Capability1.Get()
				nSuccess = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_CAPABILITY, MSG_GET, tCapability)
				IF nSuccess==0
					.Struct1.Tw_Capability1.Set(.TwainDSM1.tc_Data)
					loCapRet = .GetElementCap()
				ENDIF
		ENDWITH
		RETURN loCapRet
	ENDPROC


	PROCEDURE setcap
		LPARAMETERS tn_Code_Capability AS Long, tbValue AS Boolean
		loRetSet=NULL
		WITH THIS
			loCapab = .GetDefaultCap(tn_Code_Capability)
			IF !ISNULL(loCapab)
				DO CASE
				CASE .Struct1.Tw_Capability1.tn_Contype=5
					IF loCapab.ItemType=7
						.Struct1.Tw_OneValue1.Tw_Fix321.tn_Whole = tbValue
						.Struct1.Tw_OneValue1.Tw_Fix321.tn_Frac = 0
					ELSE
						.Struct1.Tw_OneValue1.tn_Item = tbValue
					ENDIF
					*----- Estrcutura TW_ONEVALUE
					.Struct1.Tw_OneValue1.tn_ItemType = loCapab.ItemType
					lcOneValue = .Struct1.Tw_OneValue1.Get()
					lhPointer = .GlobalAlloc(LEN(lcOneValue))
					_ptr = .GlobalLock(lhPointer)
					bOk = .RtlMoveMemory(_ptr, lcOneValue)
				CASE .Struct1.Tw_Capability1.tn_Contype=6
					.Struct1.Tw_Array1.tn_ItemType = loCapab.ItemType
					.Struct1.Tw_Array1.tn_NumItems = tbValue
				ENDCASE
				*----- Estrcutura TW_CAPABILITY
				.Struct1.Tw_Capability1.tn_Cap = tn_Code_Capability
				.Struct1.Tw_Capability1.tn_HContainer = _ptr
				tCapability = .Struct1.Tw_Capability1.Get()
				*----- Configuramos el dispositivo
				nSuccess = .TwainDSM1.DSM_Entry(.tp_Appid, .tp_SourceID, DG_CONTROL, DAT_CAPABILITY, MSG_SET, tCapability)
				IF nSuccess=0
					.Struct1.Tw_Capability1.Set(.TwainDSM1.tc_Data)
					loRetSet = .GetCurrentCap(tn_Code_Capability)
				ELSE
					MESSAGEBOX("Valor incorrectos para la capacidad", 16, _SCREEN.Caption)
				ENDIF
				.GlobalFree(_ptr)
				.GlobalFree(lhPointer)
			ELSE
				MESSAGEBOX("Capacidad no soportada o no valida", 16, _SCREEN.Caption)
			ENDIF
		ENDWITH
		RETURN loRetSet
	ENDPROC


	PROCEDURE iscapsupported
		LPARAMETERS tn_Capability_ID AS Long
		loObjSup = NULL
		IF VARTYPE(tn_Capability_ID)=="N"
			WITH THIS
				*----- Estrcutura TW_CAPABILITY
				.Struct1.Tw_Capability1.tn_Cap = tn_Capability_ID
				.Struct1.Tw_Capability1.tn_Contype = TWON_DONTCARE8
				.Struct1.Tw_Capability1.tn_HContainer = 0
				tCapability = .Struct1.Tw_Capability1.Get()
				*----- Verificamos si el dispositivo soporta una capacidad
				nSuccess = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_CAPABILITY, MSG_QUERYSUPPORT, tCapability)
				IF nSuccess==0
					.Struct1.Tw_Capability1.Set(.TwainDSM1.tc_Data)
					loObjSup = .GetElementCap()
				ENDIF
			ENDWITH
		ENDIF
		RETURN loObjSup
	ENDPROC


	PROCEDURE getcurrentcap
		LPARAMETERS tn_Capability_ID AS Long
		loObjRet = NULL
		IF VARTYPE(tn_Capability_ID)=="N"
			WITH THIS
				.Struct1.Tw_Capability1.tn_Cap = tn_Capability_ID
				.Struct1.Tw_Capability1.tn_Contype = TWON_DONTCARE8
				.Struct1.Tw_Capability1.tn_HContainer = 0
				tCapability = .Struct1.Tw_Capability1.Get()
				nSuccess = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_CAPABILITY, MSG_GETCURRENT, tCapability)
				IF nSuccess==0
					.Struct1.Tw_Capability1.Set(.TwainDSM1.tc_Data)
					loObjRet = .GetElementCap()
				ENDIF
			ENDWITH
		ENDIF
		RETURN loObjRet
	ENDPROC


	PROCEDURE getdefaultcap
		LPARAMETERS tn_Capability_ID AS Long
		loObjRet = NULL
		IF VARTYPE(tn_Capability_ID)=="N"
			WITH THIS
				.Struct1.Tw_Capability1.tn_Cap = tn_Capability_ID
				.Struct1.Tw_Capability1.tn_Contype = TWON_DONTCARE8
				.Struct1.Tw_Capability1.tn_HContainer = 0
				tCapability = .Struct1.Tw_Capability1.Get()
				nSuccess = .TwainDSM1.DSM_Entry(.tp_AppID, .tp_SourceID, DG_CONTROL, DAT_CAPABILITY, MSG_GETDEFAULT, tCapability)
				IF nSuccess==0
					.Struct1.Tw_Capability1.Set(.TwainDSM1.tc_Data)
					loObjRet = .GetElementCap()
				ENDIF
			ENDWITH
		ENDIF
		RETURN loObjRet
	ENDPROC


	PROCEDURE getelementcap
		loCapRet = NULL
		WITH THIS
			IF .Struct1.Tw_Capability1.tn_HContainer>0
				loCapRet = CREATEOBJECT("EMPTY")
				ADDPROPERTY(loCapRet, "Id", .Struct1.Tw_Capability1.tn_Cap)
				ADDPROPERTY(loCapRet, "Description", .GetNameCap(loCapRet.Id))
				ADDPROPERTY(loCapRet, "CapType", .Struct1.Tw_Capability1.tn_Contype)
				DO CASE
				CASE .Struct1.Tw_Capability1.tn_Contype=5
					*---- Is ONE VALUE
					LB_TW_ONEVALUE = SYS(2600, CTOBIN(SYS(2600, .Struct1.Tw_Capability1.tn_HContainer, 4), "4RS"), 10)
					.Struct1.Tw_OneValue1.Set(LB_TW_ONEVALUE)
					ADDPROPERTY(loCapRet, "ItemType", .Struct1.Tw_OneValue1.tn_ItemType)
					ADDPROPERTY(loCapRet, "Item", .Struct1.Tw_OneValue1.tn_Item)
				CASE .Struct1.Tw_Capability1.tn_Contype=6
					*---- Is RANGE
					LB_TW_RANGE = SYS(2600, CTOBIN(SYS(2600, .Struct1.Tw_Capability1.tn_HContainer, 4), "4RS"), 24)
					.Struct1.Tw_Range1.Set(LB_TW_RANGE)
					ADDPROPERTY(loCapRet, "ItemType", .Struct1.Tw_Range1.tn_ItemType)
					ADDPROPERTY(loCapRet, "MinValue", .Struct1.Tw_Range1.tn_MinValue)
					ADDPROPERTY(loCapRet, "MaxValue", .Struct1.Tw_Range1.tn_MaxValue)
					ADDPROPERTY(loCapRet, "StepSize", .Struct1.Tw_Range1.tn_StepSize)
					ADDPROPERTY(loCapRet, "DefaultValue", .Struct1.Tw_Range1.tn_DefaultValue)
					ADDPROPERTY(loCapRet, "CurrentValue", .Struct1.Tw_Range1.tn_CurrentValue)
				CASE .Struct1.Tw_Capability1.tn_Contype=3
					*---- Is ARRAY
					LB_TW_ARRAY = SYS(2600, CTOBIN(SYS(2600, .Struct1.Tw_Capability1.tn_HContainer, 4), "4RS"), 8)
					.Struct1.Tw_Array1.Set(LB_TW_ARRAY)
					ADDPROPERTY(loCapRet, "ItemType", .Struct1.Tw_Array1.tn_ItemType)
					ADDPROPERTY(loCapRet, "NumItems", .Struct1.Tw_Array1.tn_NumItems)
					ADDPROPERTY(loCapRet, "ItemList", .Struct1.Tw_Array1.tn_ItemList)
				CASE .Struct1.Tw_Capability1.tn_Contype=4
					*---- Is ENUM
					LB_TW_ENUM = SYS(2600, CTOBIN(SYS(2600, .Struct1.Tw_Capability1.tn_HContainer, 4), "4RS"), 32)
					.Struct1.Tw_Enumeration1.Set(LB_TW_ENUM)
					ADDPROPERTY(loCapRet, "ItemType", .Struct1.Tw_Enumeration1.tn_ItemType)
					ADDPROPERTY(loCapRet, "NumItems", .Struct1.Tw_Enumeration1.tn_NumItems)
					ADDPROPERTY(loCapRet, "CurrentIndex", .Struct1.Tw_Enumeration1.tn_CurrentIndex)
					ADDPROPERTY(loCapRet, "DefaultIndex", .Struct1.Tw_Enumeration1.tn_DefaultIndex)
					ADDPROPERTY(loCapRet, "ItemList", .Struct1.Tw_Enumeration1.tn_ItemList)
				ENDCASE
			ENDIF
		ENDWITH
		RETURN loCapRet
	ENDPROC


	PROCEDURE getcollectioncap
		*------ Recupera todas las capacidades que soporta el dispostivo de origen, 
		*------ devuelve un objeto Colección con todas las capacidades
		loColeCap=null
		WITH THIS
			IF .tp_Appid>0 AND .tp_SourceID>0
				loColeCap = CREATEOBJECT("COLLECTION")
				nCapab = 0
				FOR EACH lnCap IN .Capabilitys1
					loObj = .GetDefaultCap(lnCap)
					IF !ISNULL(loObj)
						loColeCap.Add(loObj)
					ENDIF
				NEXT
			ELSE
				MESSAGEBOX("No hay una fuente de datos abierta", 16, _SCREEN.Caption)
			ENDIF
		ENDWITH
		RETURN loColeCap
	ENDPROC


	PROCEDURE getnamecap
		LPARAMETERS tnCapability AS Long
		lcRetName = ""
		IF VARTYPE(tnCapability)=="N"
			nRec=0
			FOR EACH nEC IN THIS.Capabilitys1
				nRec = nRec + 1
				IF nEC==tnCapability
					lcRetName = THIS.Capabilitys1.GetKey(nRec)
					EXIT
				ENDIF
			NEXT
		ENDIF
		RETURN lcRetName
	ENDPROC


	PROCEDURE statustwaindsm
		nRes = 1
		WITH THIS
			IF .tp_AppID>0
				lpZero = 0
				lcHWnd = BINTOC(_SCREEN.HWnd,"4RS")
				nRes = 1
				nRes = .TwainDSM1.DSM_Entry(.tp_AppID, lpZero, DG_CONTROL, DAT_STATUS, MSG_GET, lcHWnd)
			ENDIF
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE getentrypoint
		SET STEP ON
		nRes = 1
		WITH THIS
			tpZero = 0
			lc_SourceID = SPACE(1000)
			nRes = .TwainDSM1.DSM_Entry(.tp_Appid, tpZero, DG_CONTROL, DAT_ENTRYPOINT, MSG_GET, lc_SourceID)
		ENDWITH
		RETURN nRes
	ENDPROC


	PROCEDURE Init
		WITH THIS
			IF EMPTY(.tc_FolderOut)
				*----- Si no configuro una carpeta para recibir imagenes, usamos TEMP de Windows
				.tc_FolderOut = GETENV("TEMP")
			ELSE
				IF !DIRECTORY(ALLTRIM(.tc_FolderOut))
					*----- Si no existe la carpeta para recibir imagenes, la creamos
					MD ADDBS(ALLTRIM(.tc_FolderOut))
				ENDIF
			ENDIF
		ENDWITH
	ENDPROC


	PROCEDURE removeresource
	ENDPROC


	PROCEDURE gettypecap
	ENDPROC


ENDDEFINE
*
*-- EndDefine: foxytwain
**************************************************


**************************************************
*-- Class:        struct (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 03:46:06 PM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS struct AS _kernel32


	Width = 198
	Height = 50
	Name = "struct"


	ADD OBJECT tw_identity1 AS tw_identity WITH ;
		Top = 3, ;
		Left = 5, ;
		Width = 20, ;
		Height = 20, ;
		Name = "Tw_identity1", ;
		Tw_version1.Name = "Tw_version1"


	ADD OBJECT tw_userinterface1 AS tw_userinterface WITH ;
		Top = 3, ;
		Left = 28, ;
		Width = 20, ;
		Height = 20, ;
		Name = "Tw_userinterface1"


	ADD OBJECT tw_msg1 AS tw_msg WITH ;
		Top = 3, ;
		Left = 52, ;
		Width = 20, ;
		Height = 20, ;
		Name = "Tw_msg1"


	ADD OBJECT tw_event1 AS tw_event WITH ;
		Top = 3, ;
		Left = 76, ;
		Width = 20, ;
		Height = 20, ;
		Name = "Tw_event1"


	ADD OBJECT tw_pending_xfers1 AS tw_pending_xfers WITH ;
		Top = 3, ;
		Left = 148, ;
		Name = "Tw_pending_xfers1"


	ADD OBJECT bmp_info1 AS bmp_info WITH ;
		Top = 3, ;
		Left = 100, ;
		Name = "Bmp_info1"


	ADD OBJECT bmp_header1 AS bmp_header WITH ;
		Top = 3, ;
		Left = 124, ;
		Name = "Bmp_header1"


	ADD OBJECT tw_capability1 AS tw_capability WITH ;
		Top = 3, ;
		Left = 172, ;
		Name = "Tw_capability1"


	ADD OBJECT tw_onevalue1 AS tw_onevalue WITH ;
		Top = 26, ;
		Left = 172, ;
		Name = "Tw_onevalue1", ;
		Tw_fix321.Name = "Tw_fix321"


	ADD OBJECT tw_fix321 AS tw_fix32 WITH ;
		Top = 26, ;
		Left = 148, ;
		Name = "Tw_fix321"


	ADD OBJECT tw_range1 AS tw_range WITH ;
		Top = 26, ;
		Left = 4, ;
		Name = "Tw_range1", ;
		Tw_fix321.Name = "Tw_fix321"


	ADD OBJECT tw_enumeration1 AS tw_enumeration WITH ;
		Top = 26, ;
		Left = 28, ;
		Name = "Tw_enumeration1", ;
		Tw_fix321.Name = "Tw_fix321"


	ADD OBJECT tw_array1 AS tw_array WITH ;
		Top = 26, ;
		Left = 52, ;
		Name = "Tw_array1", ;
		Tw_fix321.Name = "Tw_fix321"


	ADD OBJECT tw_imageinfo1 AS tw_imageinfo WITH ;
		Top = 26, ;
		Left = 76, ;
		Name = "Tw_imageinfo1", ;
		Tw_fix32XResol.Name = "Tw_fix32XResol", ;
		Tw_fix32YResol.Name = "Tw_fix32YResol"


	ADD OBJECT tw_imagelayout1 AS tw_imagelayout WITH ;
		Top = 26, ;
		Left = 100, ;
		Name = "Tw_imagelayout1", ;
		Tw_frame1.Tw_fix32Left.Name = "Tw_fix32Left", ;
		Tw_frame1.Tw_fix32Right.Name = "Tw_fix32Right", ;
		Tw_frame1.Tw_fix32Top.Name = "Tw_fix32Top", ;
		Tw_frame1.Tw_fix32Bottom.Name = "Tw_fix32Bottom", ;
		Tw_frame1.Name = "Tw_frame1"


	ADD OBJECT tw_frame1 AS tw_frame WITH ;
		Top = 26, ;
		Left = 123, ;
		Name = "Tw_frame1", ;
		Tw_fix32Left.Name = "Tw_fix32Left", ;
		Tw_fix32Right.Name = "Tw_fix32Right", ;
		Tw_fix32Top.Name = "Tw_fix32Top", ;
		Tw_fix32Bottom.Name = "Tw_fix32Bottom"


ENDDEFINE
*
*-- EndDefine: struct
**************************************************


**************************************************
*-- Class:        tw_array (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/08/19 11:32:01 AM
*
DEFINE CLASS tw_array AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_itemtype = 0
	*-- 4RS, 32bits, Long
	tn_numitems = 0
	*-- 1RS, 8bits, Int, Bool
	tn_itemlist = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_itemtype" type="property" display="tn_ItemType"/><memberdata name="tn_numitems" type="property" display="tn_NumItems"/><memberdata name="t_itemlist" type="property" display="t_ItemList"/><memberdata name="tn_itemlist" type="property" display="tn_ItemList"/></VFPData>]
	Name = "tw_array"


	ADD OBJECT tw_fix321 AS tw_fix32 WITH ;
		Top = 5, ;
		Left = 5, ;
		Width = 10, ;
		Height = 10, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix321"


	PROCEDURE get
		TW_ARRAY = ""
		WITH THIS
			bItemType	= BINTOC(IIF(VARTYPE(.tn_ItemType)="N", .tn_ItemType, 0), "2RS")
			bNumItems	= BINTOC(IIF(VARTYPE(.tn_ItemType)="N", .tn_ItemType, 0), "4RS")
			bItemList	= BINTOC(IIF(VARTYPE(.tn_ItemType)="N", .tn_ItemType, 0), "1RS")
			TW_ARRAY = bItemType + bNumItems + bItemList
		ENDWITH
		RETURN TW_ARRAY
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_TW_ARRAY AS String 
		IF VARTYPE(tcStruct_TW_ARRAY)=="C"
			WITH THIS
				.tn_ItemType = CTOBIN(SUBSTR(tcStruct_TW_ARRAY, 1, 2), "2RS")
				.tn_NumItems = CTOBIN(SUBSTR(tcStruct_TW_ARRAY, 3, 4), "4RS")
				.tn_ItemList = CTOBIN(SUBSTR(tcStruct_TW_ARRAY, 7, 1), "1RS")
			ENDWITH
		ENDIF
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_array
**************************************************


**************************************************
*-- Class:        tw_capability (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/05/19 03:03:06 AM
*
DEFINE CLASS tw_capability AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_cap = 0
	*-- 2RS, 16bits, Short
	tn_contype = 0
	*-- 4RS, 32bits, Long
	tn_hcontainer = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_cap" type="property" display="tn_Cap"/><memberdata name="tn_contype" type="property" display="tn_Contype"/><memberdata name="tn_hcontainer" type="property" display="tn_HContainer"/></VFPData>]
	Name = "tw_capability"


	PROCEDURE get
		TW_CAPABILITY = ""
		WITH THIS
			bCap		= BINTOC(IIF(VARTYPE(.tn_Cap)="N", .tn_Cap, 0), "2RS")
			bContype	= BINTOC(IIF(VARTYPE(.tn_Contype)="N", .tn_Contype, 0), "2RS")
			bHContainer	= BINTOC(IIF(VARTYPE(.tn_HContainer)="N", .tn_HContainer, 0), "4RS")
			TW_CAPABILITY = bCap + bConType + bHContainer
		ENDWITH
		RETURN TW_CAPABILITY
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_Tw_Capability AS String
		lBool=.F.
		IF VARTYPE(tcStruct_Tw_Capability)=="C" OR VARTYPE(tcStruct_Tw_Capability)=="Q"
			WITH THIS
				.tn_Cap			= CTOBIN(SUBSTR(tcStruct_Tw_Capability,1,2), "2RS")
				.tn_Contype 	= CTOBIN(SUBSTR(tcStruct_Tw_Capability,3,2), "2RS")
				.tn_HContainer 	= CTOBIN(SUBSTR(tcStruct_Tw_Capability,5,4), "4RS")
				lBool=.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_capability
**************************************************


**************************************************
*-- Class:        tw_enumeration (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/08/19 11:31:06 AM
*
DEFINE CLASS tw_enumeration AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_itemtype = 0
	*-- 4RS, 32bits, Long
	tn_numitems = 0
	*-- 4RS, 32bits, Long
	tn_currentindex = 0
	*-- 4RS, 32bits, Long
	tn_defaultindex = 0
	*-- 1RS, 8bits, Int, Bool
	tn_itemlist = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_currentindex" type="property" display="tn_CurrentIndex"/><memberdata name="tn_defaultindex" type="property" display="tn_DefaultIndex"/><memberdata name="tn_itemlist" type="property" display="tn_ItemList"/><memberdata name="tn_itemtype" type="property" display="tn_ItemType"/><memberdata name="tn_numitems" type="property" display="tn_NumItems"/></VFPData>]
	Name = "tw_enumeration"


	ADD OBJECT tw_fix321 AS tw_fix32 WITH ;
		Top = 5, ;
		Left = 5, ;
		Width = 10, ;
		Height = 10, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix321"


	PROCEDURE get
		TW_ENUMERATION = ""
		WITH THIS
			bItemType		= BINTOC(IIF(VARTYPE(.tn_ItemType)=="N", .tn_ItemType, 0), "2RS")
			bNumItems		= BINTOC(IIF(VARTYPE(.tn_NumItems)=="N", .tn_NumItems, 0), "4RS")
			bCurrentIndex	= BINTOC(IIF(VARTYPE(.tn_CurrentIndex)=="N", .tn_CurrentIndex, 0), "4RS")
			bDefaultIndex	= BINTOC(IIF(VARTYPE(.tn_DefaultIndex)=="N", .tn_DefaultIndex, 0), "4RS")
			bItemList		= BINTOC(IIF(VARTYPE(.tn_ItemList)=="N", .tn_ItemList, 0), "1RS")
		   TW_ENUMERATION = bItemType + bNumItems + bCurrentIndex + bDefaultIndex + bItemList
		ENDWITH
		RETURN TW_ENUMERATION
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_TW_ENUM AS String
		IF VARTYPE(tcStruct_TW_ENUM)=="C"
			WITH THIS
				.tn_ItemType 		= CTOBIN(SUBSTR(tcStruct_TW_ENUM, 1,2), "2RS")
				.tn_NumItems		= CTOBIN(SUBSTR(tcStruct_TW_ENUM, 3,4), "4RS")
				.tn_CurrentIndex 	= CTOBIN(SUBSTR(tcStruct_TW_ENUM, 7,4), "4RS")
				.tn_DefaultIndex	= CTOBIN(SUBSTR(tcStruct_TW_ENUM,11,4), "4RS")
				.tn_ItemList		= CTOBIN(SUBSTR(tcStruct_TW_ENUM,15,1), "1RS")
			ENDWITH
		ENDIF
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_enumeration
**************************************************


**************************************************
*-- Class:        tw_event (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/04/19 08:49:13 AM
*
DEFINE CLASS tw_event AS _kernel32


	Width = 20
	Height = 20
	BackColor = RGB(0,128,255)
	*-- 4RS, 32bits, Long
	tn_event = 0
	*-- 2RS, 16bits, Short
	tn_twmessage = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_event" type="property" display="tLong_Event"/><memberdata name="tshort_twmessage" type="property" display="tShort_TwMessage"/><memberdata name="tn_event" type="property" display="tn_Event"/><memberdata name="tn_twmessage" type="property" display="tn_TwMessage"/></VFPData>]
	Name = "tw_event"


	PROCEDURE get
		WITH THIS
			pEvent		= BINTOC(IIF(VARTYPE(.tn_Event)="N", .tn_Event, 0), "4RS")
			TWMessage   = BINTOC(IIF(VARTYPE(.tn_TwMessage)="N", .tn_TwMessage, 0), "2RS")
			*TWMessage   = REPLICATE(CHR(0), 60)
			TW_EVENT = pEvent + TWMessage
		ENDWITH
		RETURN TW_EVENT
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_Tw_Event AS String
		lBool=.F.
		IF VARTYPE(tcStruct_Tw_Event)=="C" OR VARTYPE(tcStruct_Tw_Event)=="Q"
			WITH THIS
				.tn_Event 		= CTOBIN(SUBSTR(tcStruct_Tw_Event,1,4), "4RS")
				.tn_TwMessage	= CTOBIN(SUBSTR(tcStruct_Tw_Event,5,2), "2RS")
				lBool=.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_event
**************************************************


**************************************************
*-- Class:        tw_fix32 (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 03:28:00 PM
*
DEFINE CLASS tw_fix32 AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_whole = 0
	*-- 2RS, 16bits, Short
	tn_frac = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_frac" type="property" display="tn_Frac"/><memberdata name="tn_whole" type="property" display="tn_Whole"/></VFPData>]
	Name = "tw_fix32"


	PROCEDURE set
		LPARAMETERS tcStruct_Tw_Fix32 AS String
		lBool=.F.
		IF VARTYPE(tcStruct_Tw_Fix32)=="C" OR VARTYPE(tcStruct_Tw_Fix32)=="Q"
			WITH THIS
				.tn_Whole = CTOBIN(SUBSTR(tcStruct_Tw_Fix32,1,2), "2RS")
				.tn_Frac  = CTOBIN(SUBSTR(tcStruct_Tw_Fix32,3,2), "2RS")
				lBool=.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


	PROCEDURE get
		TW_FIX32 = ""
		WITH THIS
			bWhole	= BINTOC(IIF(VARTYPE(.tn_Whole)="N", .tn_Whole, 0), "2RS")
			bFrac	= BINTOC(IIF(VARTYPE(.tn_Frac)="N", .tn_Frac, 0), "2RS")
			TW_FIX32 = bWhole + bFrac
		ENDWITH
		RETURN TW_FIX32
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_fix32
**************************************************


**************************************************
*-- Class:        tw_frame (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 02:36:12 PM
*
DEFINE CLASS tw_frame AS _kernel32


	Width = 20
	Height = 20
	BackColor = RGB(0,128,255)
	*-- FIX32
	tn_left = 0
	*-- FIX32
	tn_top = 0
	*-- FIX32
	tn_right = 0
	*-- FIX32
	tn_bottom = 0
	_memberdata = [<VFPData><memberdata name="tn_bottom" type="property" display="tn_Bottom"/><memberdata name="tn_left" type="property" display="tn_Left"/><memberdata name="tn_right" type="property" display="tn_Right"/><memberdata name="tn_top" type="property" display="tn_Top"/></VFPData>]
	Name = "tw_frame"


	ADD OBJECT tw_fix32left AS tw_fix32 WITH ;
		Top = 3, ;
		Left = 2, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32Left"


	ADD OBJECT tw_fix32right AS tw_fix32 WITH ;
		Top = 3, ;
		Left = 12, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32Right"


	ADD OBJECT tw_fix32top AS tw_fix32 WITH ;
		Top = 11, ;
		Left = 2, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32Top"


	ADD OBJECT tw_fix32bottom AS tw_fix32 WITH ;
		Top = 11, ;
		Left = 12, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32Bottom"


	PROCEDURE get
		TW_FRAME=""
		WITH THIS
			bLeft	 = .Tw_Fix32Left.Get()
			bRight	 = .Tw_Fix32Right.Get()
			bTop	 = .Tw_Fix32Top.Get()
			bBottom	 = .Tw_Fix32Bottom.Get()
			TW_FRAME = bLeft + bRight + bTop + bBottom
		ENDWITH
		RETURN TW_FRAME
	ENDPROC


	PROCEDURE set
		LPARAMETERS tc_Struct_Tw_Frame AS String
		lBool = .F.
		IF VARTYPE(tc_Struct_Tw_Frame)=="C"
			WITH THIS
				.Tw_Fix32Left.Set(SUBSTR(tc_Struct_Tw_Frame, 1, 4))
				.Tw_Fix32Right.Set(SUBSTR(tc_Struct_Tw_Frame, 5, 4))
				.Tw_Fix32Top.Set(SUBSTR(tc_Struct_Tw_Frame, 9, 4))
				.Tw_Fix32Bottom.Set(SUBSTR(tc_Struct_Tw_Frame,13, 4))
			ENDWITH
			lBool = .T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_frame
**************************************************


**************************************************
*-- Class:        tw_identity (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/12/19 03:42:04 PM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS tw_identity AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 4RS, 32bits, Long
	tn_id = 0
	*-- 4RS, 32bits, Long
	tn_version = 0
	*-- 4RS, 32bits, Long
	tn_supportedgroup = 1
	*-- 2RS, 16bits, Short
	tn_protocolmajor = 1
	*-- 2RS, 16bits, Short
	tn_protocolminor = 9
	*-- String 32 characters + 2 null = 34
	tc_manufacturer = "Fernando Mora"
	*-- String 32 characters + 2 null = 34
	tc_productfamily = "VFP Twain Module"
	*-- String 32 characters + 2 null = 34
	tc_productname = "FoxyTwain"
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_id" type="property" display="tLong_Id"/><memberdata name="tlong_supportedgroup" type="property" display="tLong_SupportedGroup"/><memberdata name="tlong_version" type="property" display="tLong_Version"/><memberdata name="tshort_protocolmajor" type="property" display="tShort_ProtocolMajor"/><memberdata name="tshort_protocolminor" type="property" display="tShort_ProtocolMinor"/><memberdata name="tstr32_manufacturer" type="property" display="tStr32_Manufacturer"/><memberdata name="tstr32_productfamily" type="property" display="tStr32_ProductFamily"/><memberdata name="tstr32_productname" type="property" display="tStr32_ProductName"/><memberdata name="tc_manufacturer" type="property" display="tc_Manufacturer"/><memberdata name="tc_productfamily" type="property" display="tc_ProductFamily"/><memberdata name="tc_productname" type="property" display="tc_ProductName"/><memberdata name="tn_id" type="property" display="tn_Id"/><memberdata name="tn_protocolmajor" type="property" display="tn_ProtocolMajor"/><memberdata name="tn_protocolminor" type="property" display="tn_ProtocolMinor"/><memberdata name="tn_supportedgroup" type="property" display="tn_SupportedGroup"/><memberdata name="tn_version" type="property" display="tn_Version"/></VFPData>]
	Name = "tw_identity"


	ADD OBJECT tw_version1 AS tw_version WITH ;
		Top = 5, ;
		Left = 5, ;
		Width = 10, ;
		Height = 10, ;
		BackColor = RGB(255,0,0), ;
		tn_majornum = 3, ;
		tn_minornum = 5, ;
		Name = "Tw_version1"


	PROCEDURE set
		LPARAMETERS tcStruct_Tw_Identity AS String
		lBool =.F.
		IF VARTYPE(tcStruct_Tw_Identity)=="C" OR VARTYPE(tcStruct_Tw_Identity)=="Q"
			WITH THIS
				.tn_Id				= CTOBIN(SUBSTR(tcStruct_Tw_Identity, 1, 4), "4RS")
				.Tw_Version1.Set(SUBSTR(tcStruct_Tw_Identity, 5, 42))
				.tn_ProtocolMajor	= CTOBIN(SUBSTR(tcStruct_Tw_Identity,47, 2), "2RS")
				.tn_ProtocolMinor	= CTOBIN(SUBSTR(tcStruct_Tw_Identity,49, 2), "2RS")
				.tn_SupportedGroup	= CTOBIN(SUBSTR(tcStruct_Tw_Identity,51, 2), "2RS")
				.tc_Manufacturer	= ALLTRIM(SUBSTR(tcStruct_Tw_Identity,55,34))
				.tc_ProductFamily	= ALLTRIM(SUBSTR(tcStruct_Tw_Identity,89,34))
				.tc_ProductName		= ALLTRIM(SUBSTR(tcStruct_Tw_Identity,123,34))
			ENDWITH
			lBool =.T.
		ENDIF
		RETURN lBool
	ENDPROC


	PROCEDURE get
		TW_IDENTITY = ""
		WITH THIS
			bId					= BINTOC(IIF(VARTYPE(.tn_Id)="N", .tn_Id, 0), "4RS")
			bVersion			= BINTOC(.GetHeapAlloc(.Tw_version1.Get()), "4RS")
			bProtocolMajor		= BINTOC(IIF(VARTYPE(.tn_ProtocolMajor)="N", .tn_ProtocolMajor, 0), "2RS")
			bProtocolMinor		= BINTOC(IIF(VARTYPE(.tn_ProtocolMinor)="N", .tn_ProtocolMinor, 0), "2RS")
			bSupportedGroups	= BINTOC(IIF(VARTYPE(.tn_SupportedGroup)="N", .tn_SupportedGroup, 0), "4RS")
			bManufacturer		= PADR(IIF(VARTYPE(.tc_Manufacturer)="C", ALLTRIM(SUBSTR(.tc_Manufacturer,1,32)), ""), 34, CHR(0))
			bProductFamily		= PADR(IIF(VARTYPE(.tc_ProductFamily)="C", ALLTRIM(SUBSTR(.tc_ProductFamily,1,32)), ""), 34, CHR(0))
			bProductName		= PADR(IIF(VARTYPE(.tc_ProductName)="C", ALLTRIM(SUBSTR(.tc_ProductName,1,32)), ""), 34, CHR(0))
			TW_IDENTITY = bId + bVersion + bProtocolMajor + bProtocolMinor + bSupportedGroups + bManufacturer + bProductFamily + bProductName
		ENDWITH
		RETURN TW_IDENTITY
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_identity
**************************************************


**************************************************
*-- Class:        tw_imageinfo (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 03:27:11 PM
*
DEFINE CLASS tw_imageinfo AS _kernel32


	BackColor = RGB(0,128,255)
	*-- FIX32
	tn_xresolution = 0
	*-- FIX32
	tn_yresolution = 0
	*-- 4RS
	tn_imagewidth = 0
	*-- 4RS
	tn_imagelength = 0
	*-- 2RS
	tn_samplesperpixel = 0
	*-- 2RS
	tn_bitspersample = 0
	*-- 2RS
	tn_bitsperpixel = 0
	*-- 1RS
	tn_planar = 0
	*-- 2RS
	tn_pixeltype = 0
	*-- 2RS
	tn_compression = 0
	_memberdata = [<VFPData><memberdata name="tn_bitsperpixel" type="property" display="tn_BitsPerPixel"/><memberdata name="tn_bitspersample" type="property" display="tn_BitsPerSample"/><memberdata name="tn_compression" type="property" display="tn_Compression"/><memberdata name="tn_imagelength" type="property" display="tn_ImageLength"/><memberdata name="tn_imagewidth" type="property" display="tn_ImageWidth"/><memberdata name="tn_pixeltype" type="property" display="tn_PixelType"/><memberdata name="tn_planar" type="property" display="tn_Planar"/><memberdata name="tn_samplesperpixel" type="property" display="tn_SamplesPerPixel"/><memberdata name="tn_xresolution" type="property" display="tn_XResolution"/><memberdata name="tn_yresolution" type="property" display="tn_YResolution"/></VFPData>]
	Name = "tw_imageinfo"


	ADD OBJECT tw_fix32xresol AS tw_fix32 WITH ;
		Top = 4, ;
		Left = 4, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32XResol"


	ADD OBJECT tw_fix32yresol AS tw_fix32 WITH ;
		Top = 11, ;
		Left = 11, ;
		Width = 5, ;
		Height = 5, ;
		BorderWidth = 0, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix32YResol"


	PROCEDURE get
		TW_IMAGEINFO=""
		WITH THIS
			bXResolution	 = .Tw_Fix32XResol.Get()
			bYResolution	 = .Tw_Fix32YResol.Get()
			bImageWidth		 = BINTOC(IIF(VARTYPE(.tn_ImageWidth)="N", .tn_ImageWidth, 0), "4RS")
			bImageLength	 = BINTOC(IIF(VARTYPE(.tn_ImageLength)="N", .tn_ImageLength, 0), "4RS")
			bSamplesPerPixel = BINTOC(IIF(VARTYPE(.tn_SamplesPerPixel)="N", .tn_SamplesPerPixel, 0), "2RS")
			bBitsPerSample	 = BINTOC(IIF(VARTYPE(.tn_BitsPerSample)="N", .tn_BitsPerSample, 0), "2RS")
			bBitsPerPixel	 = BINTOC(IIF(VARTYPE(.tn_BitsPerPixel)="N", .tn_BitsPerPixel, 0), "2RS")
			bPlanar			 = BINTOC(IIF(VARTYPE(.tn_Planar)="N", .tn_Planar, 0), "1RS")
			bPixelType		 = BINTOC(IIF(VARTYPE(.tn_PixelType)="N", .tn_PixelType, 0), "2RS")
			bCompression	 = BINTOC(IIF(VARTYPE(.tn_Compression)="N", .tn_Compression, 0), "2RS")
			TW_IMAGEINFO = bXResolution + bYResolution + bImageWidth + bImageLength + bSamplesPerPixel + bBitsPerSample + bBitsPerPixel + bPlanar + bPixelType + bCompression
		ENDWITH
		RETURN TW_IMAGEINFO
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_TW_IMAGEINFO AS String
		lBool=.F.
		IF VARTYPE(tcStruct_TW_IMAGEINFO)=="C"
			WITH THIS
				.tn_XResolution		= .Tw_Fix32XResol.Set(SUBSTR(tcStruct_TW_IMAGEINFO, 1, 4))
				.tn_YResolution		= .Tw_Fix32YResol.Set(SUBSTR(tcStruct_TW_IMAGEINFO, 5, 4))
				.tn_ImageWidth		= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO, 9, 4), "4RS")
				.tn_ImageLength		= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,13, 4), "4RS")
				.tn_SamplesPerPixel	= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,17, 2), "2RS")
				.tn_BitsPerSample	= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,19, 2), "2RS")
				.tn_BitsPerPixel	= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,21, 2), "2RS")
				.tn_Planar			= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,23, 1), "1RS")
				.tn_PixelType		= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,24, 2), "2RS")
				.tn_Compression		= CTOBIN(SUBSTR(tcStruct_TW_IMAGEINFO,26, 2), "2RS")
			ENDWITH
			lBool = .T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_imageinfo
**************************************************


**************************************************
*-- Class:        tw_imagelayout (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 02:45:11 PM
*
DEFINE CLASS tw_imagelayout AS _kernel32


	Width = 20
	Height = 20
	BackColor = RGB(0,128,255)
	*-- TW_FRAME
	tn_frame = 0
	*-- 4R, UINT32, Long
	tn_documentnumber = 0
	tn_pagenumber = "4R"
	tn_framenumber = "4R"
	_memberdata = [<VFPData><memberdata name="tn_documentnumber" type="property" display="tn_DocumentNumber"/><memberdata name="tn_frame" type="property" display="tn_Frame"/><memberdata name="tn_framenumber" type="property" display="tn_FrameNumber"/><memberdata name="tn_pagenumber" type="property" display="tn_PageNumber"/></VFPData>]
	Name = "tw_imagelayout"


	ADD OBJECT tw_frame1 AS tw_frame WITH ;
		Top = 2, ;
		Left = 2, ;
		Width = 16, ;
		Height = 16, ;
		Name = "Tw_frame1", ;
		Tw_fix32Left.Top = 2, ;
		Tw_fix32Left.Left = 2, ;
		Tw_fix32Left.Name = "Tw_fix32Left", ;
		Tw_fix32Right.Top = 2, ;
		Tw_fix32Right.Left = 10, ;
		Tw_fix32Right.Name = "Tw_fix32Right", ;
		Tw_fix32Top.Top = 8, ;
		Tw_fix32Top.Left = 2, ;
		Tw_fix32Top.Name = "Tw_fix32Top", ;
		Tw_fix32Bottom.Top = 8, ;
		Tw_fix32Bottom.Left = 10, ;
		Tw_fix32Bottom.Name = "Tw_fix32Bottom"


	PROCEDURE set
		LPARAMETERS tcStruct_TW_IMAGELAYOUT AS String 
		lBool = .F.
		IF VARTYPE(tcStruct_TW_IMAGELAYOUT)=="C"
			WITH THIS
				.tn_Frame 			= .Tw_Frame1.Set(SUBSTR(tcStruct_TW_IMAGELAYOUT, 1, 4))
				.tn_DocumentNumber 	= CTOBIN(SUBSTR(tcStruct_TW_IMAGELAYOUT, 5, 4), "4RS")
				.tn_PageNumber		= CTOBIN(SUBSTR(tcStruct_TW_IMAGELAYOUT, 9, 4), "4RS")
				.tn_FrameNumber		= CTOBIN(SUBSTR(tcStruct_TW_IMAGELAYOUT,13, 4), "4RS")
			ENDWITH
			lBool = .T.
		ENDIF
		RETURN lBool
	ENDPROC


	PROCEDURE get
		TW_IMAGELAYOUT=""
		WITH THIS
			bFrame			= .Tw_Frame1.Get()
			bDocumentNumber	= BINTOC(IIF(VARTYPE(.tn_DocumentNumber)="N", .tn_DocumentNumber, 0), "4RS")
			bPageNumber		= BINTOC(IIF(VARTYPE(.tn_PageNumber)="N", .tn_PageNumber, 0), "4RS")
			bFrameNumber	= BINTOC(IIF(VARTYPE(.tn_FrameNumber)="N", .tn_FrameNumber, 0), "4RS")
			TW_IMAGELAYOUT = bFrame + bDocumentNumber + bPageNumber + bFrameNumber
		ENDWITH
		RETURN TW_IMAGELAYOUT
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_imagelayout
**************************************************


**************************************************
*-- Class:        tw_msg (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 08:55:10 PM
*
DEFINE CLASS tw_msg AS _kernel32


	Width = 20
	Height = 20
	BackColor = RGB(0,128,255)
	*-- 4RS, 32bits, Long
	tn_hwnd = 0
	*-- 4RS, 32bits, Long
	tn_message = 0
	*-- 4RS, 32bits, Long
	tn_wparam = 0
	*-- 4RS, 32bits, Long
	tn_lparam = 0
	*-- 4RS, 32bits, Long
	tn_time = 0
	*-- 4RS, 32bits, Long
	tn_pt = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="tlong_hwnd" type="property" display="tLong_Hwnd"/><memberdata name="tlong_lparam" type="property" display="tLong_Lparam"/><memberdata name="tlong_message" type="property" display="tLong_Message"/><memberdata name="tlong_pt" type="property" display="tLong_Pt"/><memberdata name="tlong_time" type="property" display="tLong_Time"/><memberdata name="tlong_wparam" type="property" display="tLong_Wparam"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_hwnd" type="property" display="tn_Hwnd"/><memberdata name="tn_lparam" type="property" display="tn_Lparam"/><memberdata name="tn_message" type="property" display="tn_Message"/><memberdata name="tn_pt" type="property" display="tn_Pt"/><memberdata name="tn_time" type="property" display="tn_Time"/><memberdata name="tn_wparam" type="property" display="tn_Wparam"/></VFPData>]
	Name = "tw_msg"


	PROCEDURE get
		WITH THIS
		    bhwnd		= BINTOC(IIF(VARTYPE(.tn_Hwnd)="N", .tn_Hwnd, 0), "4RS")
		    bmessage	= BINTOC(IIF(VARTYPE(.tn_Message)="N", .tn_Message, 0), "4RS")
		    bwParam		= BINTOC(IIF(VARTYPE(.tn_Wparam)="N", .tn_Wparam, 0), "4RS")
		    blParam		= BINTOC(IIF(VARTYPE(.tn_Lparam)="N", .tn_Lparam, 0), "4RS")
		    btime		= BINTOC(IIF(VARTYPE(.tn_Time)="N", .tn_Time, 0), "4RS")
		    bpt			= BINTOC(IIF(VARTYPE(.tn_Pt)="N", .tn_Pt, 0), "4RS")
		    MSG = bhwnd + bmessage + bwParam + blParam + btime + bpt
		ENDWITH
		RETURN MSG
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_WS_MSG AS String
		lBool = .F.
		IF VARTYPE(tcStruct_WS_MSG)=="C" OR VARTYPE(tcStruct_WS_MSG)=="Q"
			WITH THIS
				.tn_Hwnd	= CTOBIN(SUBSTR(tcStruct_WS_MSG, 1, 4), "4RS")
				.tn_Message = CTOBIN(SUBSTR(tcStruct_WS_MSG, 5, 4), "4RS")
				.tn_Wparam	= CTOBIN(SUBSTR(tcStruct_WS_MSG, 9, 4), "4RS")
				.tn_Lparam	= CTOBIN(SUBSTR(tcStruct_WS_MSG,13, 4), "4RS")
				.tn_Time	= CTOBIN(SUBSTR(tcStruct_WS_MSG,17, 4), "4RS")
				.tn_Pt		= CTOBIN(SUBSTR(tcStruct_WS_MSG,21, 4), "4RS")
			ENDWITH
			lBool = .T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_msg
**************************************************


**************************************************
*-- Class:        tw_onevalue (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/08/19 11:23:13 AM
*
DEFINE CLASS tw_onevalue AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_itemtype = 0
	*-- 4RS, 32bits, Long
	tn_item = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_item1" type="property" display="tn_Item1"/><memberdata name="tn_item2" type="property" display="tn_Item2"/><memberdata name="tn_itemtype" type="property" display="tn_ItemType"/><memberdata name="tn_item" type="property" display="tn_Item"/></VFPData>]
	Name = "tw_onevalue"


	ADD OBJECT tw_fix321 AS tw_fix32 WITH ;
		Top = 5, ;
		Left = 5, ;
		Width = 10, ;
		Height = 10, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix321"


	PROCEDURE get
		TW_ONEVALUE = ""
		WITH THIS
			bItemType	= BINTOC(IIF(VARTYPE(.tn_ItemType )="N", .tn_ItemType , 0), "2RS")
			bItem1		= ""
			DO CASE
			CASE .tn_ItemType=0 or .tn_ItemType=3
				*-- TWTY_INT8 / TWTY_UINT8
				bItem = BINTOC(IIF(VARTYPE(.tn_Item)="N", .tn_Item, 0), "1RS")
			CASE .tn_ItemType=1 or .tn_ItemType=4
				*-- TWTY_INT16 / TWTY_UINT16
				bItem = BINTOC(IIF(VARTYPE(.tn_Item)="N", .tn_Item, 0), "2RS")
			CASE .tn_ItemType=2 or .tn_ItemType=5
				*-- TWTY_INT32 / TWTY_UINT32
				bItem = BINTOC(IIF(VARTYPE(.tn_Item)="N", .tn_Item, 0), "4RS")
			CASE .tn_ItemType=6
				*-- TWTY_BOOL
				bItem = BINTOC(IIF(VARTYPE(.tn_Item)="N", .tn_Item, 0), "4RS")
			CASE .tn_ItemType=7
				*-- TWTY_FIX32
				bItem = .Tw_Fix321.Get()
			ENDCASE
			TW_ONEVALUE = bItemType + bItem
		ENDWITH
		RETURN TW_ONEVALUE
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_Tw_OneValue AS String
		lBool=.F.
		IF VARTYPE(tcStruct_Tw_OneValue)=="C" OR VARTYPE(tcStruct_Tw_OneValue)=="Q"
			WITH THIS
				.tn_ItemType = CTOBIN(SUBSTR(tcStruct_Tw_OneValue,1,2), "2RS")
				DO CASE
				CASE .tn_ItemType=0 OR .tn_ItemType=3
					*-- TWTY_INT8 / TWTY_UINT8
					.tn_Item = CTOBIN(SUBSTR(tcStruct_Tw_OneValue,3,1), "1RS")
				CASE .tn_ItemType=1 OR .tn_ItemType=4
					*-- TWTY_INT16 / TWTY_UINT16
					.tn_Item = CTOBIN(SUBSTR(tcStruct_Tw_OneValue,3,2), "2RS")
				CASE .tn_ItemType=2 OR .tn_ItemType=5
					*-- TWTY_INT32 / TWTY_UINT32
					.tn_Item = CTOBIN(SUBSTR(tcStruct_Tw_OneValue,3,4), "4RS")
				CASE .tn_ItemType=6
					*-- TWTY_BOOL
					.tn_Item = CTOBIN(SUBSTR(tcStruct_Tw_OneValue,3,4), "4RS")
				CASE .tn_ItemType=7
					*-- TWTY_FIX32
					.Tw_Fix321.Set(SUBSTR(tcStruct_Tw_OneValue,3,4))
					.tn_Item = CTOBIN(.Tw_Fix321.Get(), "4RS")
				ENDCASE
				lBool=.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_onevalue
**************************************************


**************************************************
*-- Class:        tw_pending_xfers (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 08:58:10 PM
*
DEFINE CLASS tw_pending_xfers AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_count = 0
	*-- 4RS, 32bits, Long
	tn_reserved1 = 0
	*-- 4RS, 32bits, Long
	tn_reserved2 = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_reserved1" type="property" display="tLong_Reserved1"/><memberdata name="tlong_reserved2" type="property" display="tLong_Reserved2"/><memberdata name="tshort_count" type="property" display="tShort_Count"/><memberdata name="tn_count" type="property" display="tn_Count"/><memberdata name="tn_reserved1" type="property" display="tn_Reserved1"/><memberdata name="tn_reserved2" type="property" display="tn_Reserved2"/></VFPData>]
	Name = "tw_pending_xfers"


	PROCEDURE get
		TW_PENDINGXFERS = ""
		WITH THIS
			bCount		= BINTOC(IIF(VARTYPE(.tn_Count)=="N", .tn_Count, 0), "2RS")
			bReserved1	= BINTOC(IIF(VARTYPE(.tn_Reserved1)=="N", .tn_Reserved1, 0), "4RS")
			bReserved2	= BINTOC(IIF(VARTYPE(.tn_Reserved2)=="N", .tn_Reserved2, 0), "4RS")
			TW_PENDINGXFERS = bCount + bReserved1 + bReserved2
		ENDWITH
		RETURN TW_PENDINGXFERS
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_TW_PENDINGXFERS AS String
		lBool=.F.
		IF VARTYPE(tcStruct_TW_PENDINGXFERS)=="C" OR VARTYPE(tcStruct_TW_PENDINGXFERS)=="Q"
			WITH THIS
				.tn_Count	  = CTOBIN(SUBSTR(tcStruct_TW_PENDINGXFERS,1,2), "2RS")
				.tn_Reserved1 = CTOBIN(SUBSTR(tcStruct_TW_PENDINGXFERS,3,4), "4RS")
				.tn_Reserved2 = CTOBIN(SUBSTR(tcStruct_TW_PENDINGXFERS,7,4), "4RS")
				lBool=.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_pending_xfers
**************************************************


**************************************************
*-- Class:        tw_range (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/08/19 11:32:11 AM
*
DEFINE CLASS tw_range AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_itemtype = 0
	*-- 4RS, 32bits, Long
	tn_minvalue = 0
	*-- 4RS, 32bits, Long
	tn_maxvalue = 0
	*-- 4RS, 32bits, Long
	tn_stepsize = 0
	*-- 4RS, 32bits, Long
	tn_defaultvalue = 0
	*-- 4RS, 32bits, Long
	tn_currentvalue = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tn_currentvalue" type="property" display="tn_CurrentValue"/><memberdata name="tn_defaultvalue" type="property" display="tn_DefaultValue"/><memberdata name="tn_itemtype" type="property" display="tn_ItemType"/><memberdata name="tn_maxvalue" type="property" display="tn_MaxValue"/><memberdata name="tn_minvalue" type="property" display="tn_MinValue"/><memberdata name="tn_stepsize" type="property" display="tn_StepSize"/></VFPData>]
	Name = "tw_range"


	ADD OBJECT tw_fix321 AS tw_fix32 WITH ;
		Top = 5, ;
		Left = 5, ;
		Width = 10, ;
		Height = 10, ;
		BackColor = RGB(255,0,0), ;
		Name = "Tw_fix321"


	PROCEDURE get
		TW_RANGE = ""
		WITH THIS
			bItemType		= BINTOC(IIF(VARTYPE(.tn_ItemType)=="N", .tn_ItemType, 0), "2RS") 
			bMinValue		= BINTOC(IIF(VARTYPE(.tn_MinValue)=="N", .tn_MinValue, 0), "4RS") 
			bMaxValue		= BINTOC(IIF(VARTYPE(.tn_MaxValue)=="N", .tn_MaxValue, 0), "4RS") 
			bStepSize		= BINTOC(IIF(VARTYPE(.tn_StepSize)=="N", .tn_StepSize, 0), "4RS") 
			bDefaultValue	= BINTOC(IIF(VARTYPE(.tn_DefaultValue)=="N", .tn_DefaultValue, 0), "4RS") 
			bCurrentValue	= BINTOC(IIF(VARTYPE(.tn_CurrentValue)=="N", .tn_CurrentValue, 0), "4RS") 
			TW_RANGE = bItemType + bMinValue + bMaxValue + bStepSize + bDefaultValue + bCurrentValue
		ENDWITH
		RETURN TW_RANGE
	ENDPROC


	PROCEDURE set
		LPARAMETERS tc_struct_TW_RANGE AS String
		lBool=.F.
		IF VARTYPE(tc_struct_TW_RANGE)=="C" OR VARTYPE(tc_struct_TW_RANGE)=="Q"
			WITH THIS
				.tn_ItemType	 = CTOBIN(SUBSTR(tc_struct_TW_RANGE,1,2), "2RS")
				.tn_MinValue	 = CTOBIN(SUBSTR(tc_struct_TW_RANGE,3,4), "4RS")
				.tn_MaxValue	 = CTOBIN(SUBSTR(tc_struct_TW_RANGE,7,4), "4RS")
				.tn_StepSize	 = CTOBIN(SUBSTR(tc_struct_TW_RANGE,11,4), "4RS")
				.tn_DefaultValue = CTOBIN(SUBSTR(tc_struct_TW_RANGE,15,4), "4RS")
				.tn_CurrentValue = CTOBIN(SUBSTR(tc_struct_TW_RANGE,19,4), "4RS")
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_range
**************************************************


**************************************************
*-- Class:        tw_setupfilexfer (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/11/19 04:33:06 PM
*
DEFINE CLASS tw_setupfilexfer AS _kernel32


	BackColor = RGB(0,128,255)
	*-- 255STR
	tc_filename = ""
	*-- 2RS
	tn_format = 2
	*-- 4RS
	tn_vrefnum = 65535
	_memberdata = [<VFPData><memberdata name="tc_filename" type="property" display="tc_FileName"/><memberdata name="tn_format" type="property" display="tn_Format"/><memberdata name="tn_vrefnum" type="property" display="tn_VRefNum"/></VFPData>]
	Name = "tw_setupfilexfer"


	PROCEDURE set
		LPARAMETERS tcStruct_TW_SETUPFILEXFER AS String
		lBool=.F.
		IF VARTYPE(tcStruct_TW_SETUPFILEXFER)=="C"
			WITH THIS
				.tc_FileName = ALLTRIM(SUBSTR(tcStruct_TW_SETUPFILEXFER, 1, 255))
				.tn_Format	 = CTOBIN(SUBSTR(tcStruct_TW_SETUPFILEXFER,256,2), "2RS")
				.tn_VRefNum	 = CTOBIN(SUBSTR(tcStruct_TW_SETUPFILEXFER,258,4), "4RS")
			ENDWITH
			lBool=.T.
		ENDIF
		RETURN lBool
	ENDPROC


	PROCEDURE get
		TW_SETUPFILEXFER=""
		WITH THIS
			bFileName = PADR(ALLTRIM(.tc_FileName),256, CHR(0))
			bFormat	  = BINTOC(IIF(VARTYPE(.tn_Format), .tn_Format, 0), "2RS")
			bVRefNum  = BINTOC(IIF(VARTYPE(.bVRefNum), .bVRefNum, 0), "4RS")
			TW_SETUPFILEXFER = bFileName + bFormat + bVRefNum
		ENDWITH
		RETURN TW_SETUPFILEXFER
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_setupfilexfer
**************************************************


**************************************************
*-- Class:        tw_userinterface (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 09:00:13 PM
*
DEFINE CLASS tw_userinterface AS _kernel32


	Width = 20
	Height = 20
	BackColor = RGB(0,128,255)
	*-- 2RS, 16bits, Short
	tn_showui = 1
	*-- 2RS, 16bits, Short
	tn_modalui = 1
	*-- 4RS, 32bits, Long
	tn_parent = 0
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tlong_parent" type="property" display="tLong_Parent"/><memberdata name="tshort_modalui" type="property" display="tShort_ModalUI"/><memberdata name="tshort_showui" type="property" display="tShort_ShowUI"/><memberdata name="tn_modalui" type="property" display="tn_ModalUI"/><memberdata name="tn_parent" type="property" display="tn_Parent"/><memberdata name="tn_showui" type="property" display="tn_ShowUI"/></VFPData>]
	Name = "tw_userinterface"


	PROCEDURE get
		WITH THIS
			bShowUI		= BINTOC(IIF(VARTYPE(.tn_ShowUI)="N", .tn_ShowUI, 0), "2RS")
			bModalUI	= BINTOC(IIF(VARTYPE(.tn_ModalUI)="N", .tn_ModalUI, 0), "2RS")
			bhParent	= BINTOC(IIF(VARTYPE(.tn_Parent)="N", .tn_Parent, 0), "4RS")
			TW_USERINTERFACE = bShowUI + bModalUI + bhParent
		ENDWITH
		RETURN TW_USERINTERFACE
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruc_TW_USERINTERFACE AS String
		lBool =.F.
		IF VARTYPE(tcStruc_TW_USERINTERFACE)=="C" OR VARTYPE(tcStruc_TW_USERINTERFACE)=="Q"
			WITH THIS
				.tn_ShowUI	= CTOBIN(SUBSTR(tcStruc_TW_USERINTERFACE,1,2), "2RS")
				.tn_ModalUI	= CTOBIN(SUBSTR(tcStruc_TW_USERINTERFACE,3,2), "2RS")
				.tn_Parent	= CTOBIN(SUBSTR(tcStruc_TW_USERINTERFACE,5,4), "4RS")
				lBool =.T.
			ENDWITH
		ENDIF
		RETURN lBool
	ENDPROC


	PROCEDURE Init
		THIS.tn_Parent = _SCREEN.HWnd
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_userinterface
**************************************************


**************************************************
*-- Class:        tw_version (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  _kernel32 (c:\foxytwain\foxytwain.vcx)
*-- BaseClass:    container
*-- Time Stamp:   11/03/19 09:04:07 PM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS tw_version AS _kernel32


	BackColor = RGB(0,0,255)
	*-- 2RS, 16bits, Short
	tn_majornum = 1
	*-- 2RS, 16bits, Short
	tn_minornum = 0
	*-- 2RS, 16bits, Short
	tn_language = 83
	*-- 2RS, 16bits, Short
	tn_country = 593
	*-- String 32 characters + 2 null = 34
	tc_info = "FoxyTwain VFP"
	_memberdata = [<VFPData><memberdata name="get" type="method" display="Get"/><memberdata name="set" type="method" display="Set"/><memberdata name="tshort_country" type="property" display="tShort_Country"/><memberdata name="tshort_language" type="property" display="tShort_Language"/><memberdata name="tshort_majornum" type="property" display="tShort_MajorNum"/><memberdata name="tshort_minornum" type="property" display="tShort_MinorNum"/><memberdata name="tstr32_info" type="property" display="tStr32_Info"/><memberdata name="tc_info" type="property" display="tc_Info"/><memberdata name="tn_country" type="property" display="tn_Country"/><memberdata name="tn_language" type="property" display="tn_Language"/><memberdata name="tn_majornum" type="property" display="tn_MajorNum"/><memberdata name="tn_minornum" type="property" display="tn_MinorNum"/></VFPData>]
	Name = "tw_version"


	PROCEDURE get
		TW_VERSION = ""
		WITH THIS
			bMajorNum	= BINTOC(IIF(VARTYPE(.tn_MajorNum)="N", .tn_MajorNum, 0), "2RS")
			bMinorNum	= BINTOC(IIF(VARTYPE(.tn_MinorNum)="N", .tn_MinorNum, 0), "2RS")
			bLanguage	= BINTOC(IIF(VARTYPE(.tn_Language)="N", .tn_Language, 0), "2RS")
			bCountry	= BINTOC(IIF(VARTYPE(.tn_Country)="N", .tn_Country, 0), "2RS")
			bInfo		= IIF(VARTYPE(.tc_Info)="C", PADR(ALLTRIM(SUBSTR(.tc_Info,1,32)), 34, CHR(0)), REPLICATE(CHR(0),34))
			TW_VERSION	= bMajorNum + bMinorNum + bLanguage + bCountry + bInfo
		ENDWITH
		RETURN TW_VERSION
	ENDPROC


	PROCEDURE set
		LPARAMETERS tcStruct_TwIdentity AS String
		lBool =.F.
		IF VARTYPE(tcStruct_TwIdentity)=="C" OR VARTYPE(tcStruct_TwIdentity)=="Q"
			WITH THIS
				.tn_MajorNum = CTOBIN(SUBSTR(tcStruct_TwIdentity, 1, 2), "2RS")
				.tn_MinorNum = CTOBIN(SUBSTR(tcStruct_TwIdentity, 3, 2), "2RS")
				.tn_Language = CTOBIN(SUBSTR(tcStruct_TwIdentity, 5, 2), "2RS")
				.tn_Country	 = CTOBIN(SUBSTR(tcStruct_TwIdentity,7, 2), "2RS")
				.tc_Info 	 = ALLTRIM(SUBSTR(tcStruct_TwIdentity,9, 34))
			ENDWITH
			lBool = .T.
		ENDIF
		RETURN lBool
	ENDPROC


ENDDEFINE
*
*-- EndDefine: tw_version
**************************************************


**************************************************
*-- Class:        capabilitys (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  collection
*-- BaseClass:    collection
*-- Time Stamp:   11/06/19 09:35:09 PM
*
DEFINE CLASS capabilitys AS collection


	Height = 23
	Width = 23
	Name = "capabilitys"


	PROCEDURE Init
		WITH THIS
			*.Add(0x8000,"CAP_CUSTOMBASE")
			.Add(0x0001,"CAP_XFERCOUNT")
			.Add(0x0100,"ICAP_COMPRESSION")
			.Add(0x0101,"ICAP_PIXELTYPE")
			.Add(0x0102,"ICAP_UNITS")
			.Add(0x0103,"ICAP_XFERMECH")
			.Add(0x1000,"CAP_AUTHOR")
			.Add(0x1001,"CAP_CAPTION")
			.Add(0x1002,"CAP_FEEDERENABLED")
			.Add(0x1003,"CAP_FEEDERLOADED")
			.Add(0x1004,"CAP_TIMEDATE")
			.Add(0x1005,"CAP_SUPPORTEDCAPS")
			.Add(0x1006,"CAP_EXTENDEDCAPS")
			.Add(0x1007,"CAP_AUTOFEED")
			.Add(0x1008,"CAP_CLEARPAGE")
			.Add(0x1009,"CAP_FEEDPAGE")
			.Add(0x100a,"CAP_REWINDPAGE")
			.Add(0x100b,"CAP_INDICATORS")
			.Add(0x100d,"CAP_PAPERDETECTABLE")
			.Add(0x100e,"CAP_UICONTROLLABLE")
			.Add(0x100f,"CAP_DEVICEONLINE")
			.Add(0x1010,"CAP_AUTOSCAN")
			.Add(0x1011,"CAP_THUMBNAILSENABLED")
			.Add(0x1012,"CAP_DUPLEX")
			.Add(0x1013,"CAP_DUPLEXENABLED")
			.Add(0x1014,"CAP_ENABLEDSUIONLY")
			.Add(0x1015,"CAP_CUSTOMDSDATA")
			.Add(0x1016,"CAP_ENDORSER")
			.Add(0x1017,"CAP_JOBCONTROL")
			.Add(0x1018,"CAP_ALARMS")
			.Add(0x1019,"CAP_ALARMVOLUME")
			.Add(0x101a,"CAP_AUTOMATICCAPTURE")
			.Add(0x101b,"CAP_TIMEBEFOREFIRSTCAPTURE")
			.Add(0x101c,"CAP_TIMEBETWEENCAPTURES")
			.Add(0x101d,"CAP_CLEARBUFFERS")
			.Add(0x101e,"CAP_MAXBATCHBUFFERS")
			.Add(0x101f,"CAP_DEVICETIMEDATE")
			.Add(0x1020,"CAP_POWERSUPPLY")
			.Add(0x1021,"CAP_CAMERAPREVIEWUI")
			.Add(0x1022,"CAP_DEVICEEVENT")
			.Add(0x1024,"CAP_SERIALNUMBER")
			.Add(0x1026,"CAP_PRINTER")
			.Add(0x1027,"CAP_PRINTERENABLED")
			.Add(0x1028,"CAP_PRINTERINDEX")
			.Add(0x1029,"CAP_PRINTERMODE")
			.Add(0x102a,"CAP_PRINTERSTRING")
			.Add(0x102b,"CAP_PRINTERSUFFIX")
			.Add(0x102c,"CAP_LANGUAGE")
			.Add(0x102d,"CAP_FEEDERALIGNMENT")
			.Add(0x102e,"CAP_FEEDERORDER")
			.Add(0x1030,"CAP_REACQUIREALLOWED")
			.Add(0x1032,"CAP_BATTERYMINUTES")
			.Add(0x1033,"CAP_BATTERYPERCENTAGE")
			.Add(0x1034,"CAP_CAMERASIDE")
			.Add(0x1035,"CAP_SEGMENTED")
			.Add(0x1036,"CAP_CAMERAENABLED")
			.Add(0x1037,"CAP_CAMERAORDER")
			.Add(0x1038,"CAP_MICRENABLED")
			.Add(0x1039,"CAP_FEEDERPREP")
			.Add(0x103a,"CAP_FEEDERPOCKET")
			.Add(0x103b,"CAP_AUTOMATICSENSEMEDIUM")
			.Add(0x103c,"CAP_CUSTOMINTERFACEGUID")
			.Add(0x103d,"CAP_SUPPORTEDCAPSSEGMENTUNIQUE")
			.Add(0x103e,"CAP_SUPPORTEDDATS")
			.Add(0x103f,"CAP_DOUBLEFEEDDETECTION")
			.Add(0x1040,"CAP_DOUBLEFEEDDETECTIONLENGTH")
			.Add(0x1041,"CAP_DOUBLEFEEDDETECTIONSENSITIVITY")
			.Add(0x1042,"CAP_DOUBLEFEEDDETECTIONRESPONSE")
			.Add(0x1043,"CAP_PAPERHANDLING")
			.Add(0x1044,"CAP_INDICATORSMODE")
			.Add(0x1045,"CAP_PRINTERVERTICALOFFSET")
			.Add(0x1046,"CAP_POWERSAVETIME")
			.Add(0x1047,"CAP_PRINTERCHARROTATION")
			.Add(0x1048,"CAP_PRINTERFONTSTYLE")
			.Add(0x1049,"CAP_PRINTERINDEXLEADCHAR")
			.Add(0x104A,"CAP_PRINTERINDEXMAXVALUE")
			.Add(0x104B,"CAP_PRINTERINDEXNUMDIGITS")
			.Add(0x104C,"CAP_PRINTERINDEXSTEP")
			.Add(0x104D,"CAP_PRINTERINDEXTRIGGER")
			.Add(0x104E,"CAP_PRINTERSTRINGPREVIEW")
			.Add(0x1100,"ICAP_AUTOBRIGHT")
			.Add(0x1101,"ICAP_BRIGHTNESS")
			.Add(0x1103,"ICAP_CONTRAST")
			.Add(0x1104,"ICAP_CUSTHALFTONE")
			.Add(0x1105,"ICAP_EXPOSURETIME")
			.Add(0x1106,"ICAP_FILTER")
			.Add(0x1107,"ICAP_FLASHUSED")
			.Add(0x1108,"ICAP_GAMMA")
			.Add(0x1109,"ICAP_HALFTONES")
			.Add(0x110a,"ICAP_HIGHLIGHT")
			.Add(0x110c,"ICAP_IMAGEFILEFORMAT")
			.Add(0x110d,"ICAP_LAMPSTATE")
			.Add(0x110e,"ICAP_LIGHTSOURCE")
			.Add(0x1110,"ICAP_ORIENTATION")
			.Add(0x1111,"ICAP_PHYSICALWIDTH")
			.Add(0x1112,"ICAP_PHYSICALHEIGHT")
			.Add(0x1113,"ICAP_SHADOW")
			.Add(0x1114,"ICAP_FRAMES")
			.Add(0x1116,"ICAP_XNATIVERESOLUTION")
			.Add(0x1117,"ICAP_YNATIVERESOLUTION")
			.Add(0x1118,"ICAP_XRESOLUTION")
			.Add(0x1119,"ICAP_YRESOLUTION")
			.Add(0x111a,"ICAP_MAXFRAMES")
			.Add(0x111b,"ICAP_TILES")
			.Add(0x111c,"ICAP_BITORDER")
			.Add(0x111d,"ICAP_CCITTKFACTOR")
			.Add(0x111e,"ICAP_LIGHTPATH")
			.Add(0x111f,"ICAP_PIXELFLAVOR")
			.Add(0x1120,"ICAP_PLANARCHUNKY")
			.Add(0x1121,"ICAP_ROTATION")
			.Add(0x1122,"ICAP_SUPPORTEDSIZES")
			.Add(0x1123,"ICAP_THRESHOLD")
			.Add(0x1124,"ICAP_XSCALING")
			.Add(0x1125,"ICAP_YSCALING")
			.Add(0x1126,"ICAP_BITORDERCODES")
			.Add(0x1127,"ICAP_PIXELFLAVORCODES")
			.Add(0x1128,"ICAP_JPEGPIXELTYPE")
			.Add(0x112a,"ICAP_TIMEFILL")
			.Add(0x112b,"ICAP_BITDEPTH")
			.Add(0x112c,"ICAP_BITDEPTHREDUCTION")
			.Add(0x112d,"ICAP_UNDEFINEDIMAGESIZE")
			.Add(0x112e,"ICAP_IMAGEDATASET")
			.Add(0x112f,"ICAP_EXTIMAGEINFO")
			.Add(0x1130,"ICAP_MINIMUMHEIGHT")
			.Add(0x1131,"ICAP_MINIMUMWIDTH")
			.Add(0x1134,"ICAP_AUTODISCARDBLANKPAGES")
			.Add(0x1136,"ICAP_FLIPROTATION")
			.Add(0x1137,"ICAP_BARCODEDETECTIONENABLED")
			.Add(0x1138,"ICAP_SUPPORTEDBARCODETYPES")
			.Add(0x1139,"ICAP_BARCODEMAXSEARCHPRIORITIES")
			.Add(0x113a,"ICAP_BARCODESEARCHPRIORITIES")
			.Add(0x113b,"ICAP_BARCODESEARCHMODE")
			.Add(0x113c,"ICAP_BARCODEMAXRETRIES")
			.Add(0x113d,"ICAP_BARCODETIMEOUT")
			.Add(0x113e,"ICAP_ZOOMFACTOR")
			.Add(0x113f,"ICAP_PATCHCODEDETECTIONENABLED")
			.Add(0x1140,"ICAP_SUPPORTEDPATCHCODETYPES")
			.Add(0x1141,"ICAP_PATCHCODEMAXSEARCHPRIORITIES")
			.Add(0x1142,"ICAP_PATCHCODESEARCHPRIORITIES")
			.Add(0x1143,"ICAP_PATCHCODESEARCHMODE")
			.Add(0x1144,"ICAP_PATCHCODEMAXRETRIES")
			.Add(0x1145,"ICAP_PATCHCODETIMEOUT")
			.Add(0x1146,"ICAP_FLASHUSED2")
			.Add(0x1147,"ICAP_IMAGEFILTER")
			.Add(0x1148,"ICAP_NOISEFILTER")
			.Add(0x1149,"ICAP_OVERSCAN")
			.Add(0x1150,"ICAP_AUTOMATICBORDERDETECTION")
			.Add(0x1151,"ICAP_AUTOMATICDESKEW")
			.Add(0x1152,"ICAP_AUTOMATICROTATE")
			.Add(0x1153,"ICAP_JPEGQUALITY")
			.Add(0x1154,"ICAP_FEEDERTYPE")
			.Add(0x1155,"ICAP_ICCPROFILE")
			.Add(0x1156,"ICAP_AUTOSIZE")
			.Add(0x1157,"ICAP_AUTOMATICCROPUSESFRAME")
			.Add(0x1158,"ICAP_AUTOMATICLENGTHDETECTION")
			.Add(0x1159,"ICAP_AUTOMATICCOLORENABLED")
			.Add(0x115a,"ICAP_AUTOMATICCOLORNONCOLORPIXELTYPE")
			.Add(0x115b,"ICAP_COLORMANAGEMENTENABLED")
			.Add(0x115c,"ICAP_IMAGEMERGE")
			.Add(0x115d,"ICAP_IMAGEMERGEHEIGHTTHRESHOLD")
			.Add(0x115e,"ICAP_SUPPORTEDEXTIMAGEINFO")
			.Add(0x115f,"ICAP_FILMTYPE")
			.Add(0x1160,"ICAP_MIRROR")
			.Add(0x1161,"ICAP_JPEGSUBSAMPLING")
			.Add(0x1202,"ACAP_XFERMECH")
		ENDWITH
	ENDPROC


ENDDEFINE
*
*-- EndDefine: capabilitys
**************************************************


**************************************************
*-- Class:        imgfile (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  collection
*-- BaseClass:    collection
*-- Time Stamp:   11/11/19 04:55:07 PM
*
DEFINE CLASS imgfile AS collection


	Height = 23
	Width = 23
	Name = "imgfile"


ENDDEFINE
*
*-- EndDefine: imgfile
**************************************************


**************************************************
*-- Class:        twaindsm (c:\foxytwain\foxytwain.vcx)
*-- ParentClass:  control
*-- BaseClass:    control
*-- Time Stamp:   11/12/19 11:28:01 AM
*
#INCLUDE "c:\foxytwain\foxytwain.h"
*
DEFINE CLASS twaindsm AS control


	Width = 11
	Height = 13
	BackColor = RGB(0,255,0)
	tc_data = ""
	*-- XML Metadata for customizable properties
	_memberdata = [<VFPData><memberdata name="dsm_entry" type="method" display="DSM_Entry"/><memberdata name="tc_data" type="property" display="tc_Data"/><memberdata name="dsm_entry2" type="method" display="DSM_Entry2"/><memberdata name="dsm_entry3" type="method" display="DSM_Entry3"/><memberdata name="tp_data" type="property" display="tp_Data"/><memberdata name="th_module" type="property" display="th_Module"/><memberdata name="tp_procaddress" type="property" display="tp_ProcAddress"/></VFPData>]
	tp_data = 0
	th_module = 0
	tp_procaddress = 0
	Name = "twaindsm"


	PROCEDURE dsm_entry
		LPARAMETERS tp_AppID AS Long, tp_Dest AS Long, tn_DG AS Long, tn_DAT AS Long, tn_MSG AS Long, tc_Data AS String
		*----- Invocamos Twain
		THIS.tc_Data=""
		nRes0 = 1
		TRY
			nRes0 = DSM_Entry(tp_AppID, tp_Dest, tn_DG, tn_DAT, tn_MSG, @tc_Data)
			THIS.tc_Data=tc_Data
		CATCH TO oError
		ENDTRY
		RETURN nRes0
	ENDPROC


	PROCEDURE Destroy
		CLEAR DLLS DSM_Entry, DSM_Entry2, DSM_Entry3
	ENDPROC


	PROCEDURE Init
		*----- DECLARATIONS API -  TWAIN_32 - TWAINDSM
		DECLARE LONG DSM_Entry IN TWAINDSM AS DSM_Entry;
			LONG	 pOrigin,;
			LONG	 pDest,;
			LONG	 DG,;
			LONG	 DAT,;
			LONG	 MSG,;
			STRING	@pData
	ENDPROC


ENDDEFINE
*
*-- EndDefine: twaindsm
**************************************************
