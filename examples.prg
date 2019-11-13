SET CLASSLIB TO C:\FOXYTWAIN\FOXYTWAIN ADDITIVE
oTwain = NULL
oTwain = CREATEOBJECT("FoxyTwain.FoxyTwain")
*----- Abrimos DSM (Administrador de origenes)
nDSM = oTwain.OpenTwainDSM()
IF nDSM<>0
	RETURN .F.
ENDIF 

*----- Seleccionar Origen
cOrigen = oTwain.SelectSource()
IF EMPTY(cOrigen)
	RETURN .F.
ENDIF

*------ Abrimos Origen
nOrigen = oTwain.OpenTwainDS()
IF nOrigen<>0
	RETURN .F.		
ENDIF

SET STEP ON 
*------ PRIMER ESCANEO, CON VALORES POR DEFECTO
*------ Transferimos imagen
lScan = oTwain.GetTranferImagen()
IF lScan=.F.
	RETURN .F.
ENDIF

*----- SEGUNDO ESCANEO, CONFIGURANDO VALORES PERSONALIZADOS
*----- Configurar resolución a 200ppp
? oTwain.SetCap(0x1118,200)
? oTwain.SetCap(0x1119,200)

*----- Configurar color de pixel a escala de grises (0 BN, 1 Grises, 2 color)
? oTwain.SetCap(0x0101,1)
lScan = oTwain.GetTranferImagen()
IF lScan=.F.
	RETURN .F.
ENDIF

? "Total Imagenes ", oTwain.ImgFile1.Count
FOR XI = 1 TO oTwain.ImgFile1.Count 
	? oTwain.ImgFile1.Item(XI)
NEXT

? oTwain.CloseTwainDS()
? oTwain.CloseTwainDSM()
? oTwain.GetHeapFree()
oTwain = NULL
RELEASE oTwain



