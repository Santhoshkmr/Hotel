CREATE OR REPLACE FUNCTION SEDS_FN_TRUPDATE(SUBMITALID  VARCHAR2,
                                              BODCUMENTID VARCHAR2,
                                              CONDITION    NUMBER)
RETURN NUMBER IS
 DISPID NUMBER;
 TARGET NUMBER;
BEGIN

  
  SELECT SD.ID INTO DISPID FROM SEDS_TA_BDSUBMITTAL BDS,SEDS_TA_SUBMITDISPOSITION SD
  WHERE BDS.SUBMITDISPOSITIONID =SD.ID AND BDS.ID=SUBMITALID;
  
  IF(DISPID =241 OR DISPID =242) THEN 
     TARGET :=1;
  ELSE    
  TARGET :=2;
  END IF;

   RETURN TARGET;
 END SEDS_FN_TRUPDATE;
