CREATE OR REPLACE FUNCTION SEDS_FN_SOPMUPDATE(SUBMITALID  VARCHAR2,
                                              BODCUMENTID VARCHAR2,
                                              CONDTION    NUMBER)
  RETURN NUMBER IS
  VERRORREC      SEDS_TA_DBERRORLOG%ROWTYPE;
  DOCTYPE        NUMBER := 0;
  ROUTESTS       NUMBER := 0;
  ISPUBLISHED    NUMBER := 0;
  TEMP1          NUMBER := 0;
  VOLID          NUMBER := 0;
  pardoc         NUMBER := 0;
  TARGET         NUMBER ;
  DISPID         NUMBER;
  LSID           NUMBER;
  PARREVID       NUMBER := 0;
  STSREJECTCHECK NUMBER := 0;
  ATLASID        NUMBER := 0;
  ISROUTATSE     NUMBER := 0;
  BPARREVID      NUMBER :=0;
  OPENDATACOUNT  NUMBER :=0;
  PARTNUMBER     VARCHAR2(200);
  BOEINGDATAID   VARCHAR2(200);
  SUBMITTYPE     VARCHAR2(5);
  REVEDATE       DATE;
  PARDISPID      NUMBER :=0;


BEGIN

  BEGIN
    SELECT BDS.PUBLISHACTIONID, BDS.LASTREVISEDBY, BDS.SUBMITDISPOSITIONID
      INTO ISPUBLISHED, LSID, STSREJECTCHECK
      FROM SEDS_TA_BDSUBMITTAL BDS
     WHERE BDS.ID = SUBMITALID;
    SELECT SS.BDOCUMENTTYPEID, SS.BVOLUMEID
      INTO DOCTYPE, VOLID
      FROM SEDS_TA_DMBOEINGDOCUMENT SS
     WHERE SS.BSUBMITTALID = SUBMITALID;
     
    SELECT COUNT(*)
      INTO ROUTESTS
      FROM SEDS_VW_SUBMITPARTMODELLIST SPM
     WHERE SPM.ROUTETOSTSIND = 'Y'
       AND SPM.BSID = SUBMITALID;
  
    SELECT COUNT(*)
      INTO PARDOC
      FROM SEDS_TA_DMBOEINGDOCUMENT SR, SEDS_TA_BOEINGDOCUMENT BD
     WHERE BD.ID = SR.BPARENTBDID
       AND SR.BSUBMITTALID = SUBMITALID;
    IF (PARDOC != 0) THEN
      SELECT BD.DOCTYPEID, SR.BPARENTBDREVISIONID
        INTO PARDOC, PARREVID
        FROM SEDS_TA_DMBOEINGDOCUMENT SR, SEDS_TA_BOEINGDOCUMENT BD
       WHERE BD.ID = SR.BPARENTBDID
         AND SR.BSUBMITTALID = SUBMITALID;

      SELECT COUNT(*)
        INTO ROUTESTS
        FROM SEDS_VW_SUBMITPARTMODELLIST SPM
       WHERE SPM.ROUTETOSTSIND = 'Y'
         AND SPM.BRID = PARREVID;
         
         --ADDED FOR US 414969
         SELECT COUNT(*)
        INTO ATLASID
        FROM SEDS_VW_SUBMITPARTMODELLIST   SPM,
             SEDS_TA_BDR2SEDSPART          B2S,
             SEDS_TA_BOEINGDATAREQUIREMENT BDR
       WHERE SPM.MASTERPARTID = B2S.SEDSPARTID
         AND B2S.BDRID = BDR.ID
         AND BDR.ACTIVEIND = 'Y'
         AND BDR.DATAREQTTYPEID IN (17, 19)
         AND SPM.BRID = PARREVID;
         
         SELECT COUNT(*)
        INTO ISROUTATSE
        FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
             SEDS_TA_BDR2SEDSPART          B2SP,
             SEDS_VW_SUBMITPARTMODELLIST   SPM
       WHERE BDR.ACTIVEIND = 'Y'
         AND BDR.VCMM2ATSEIND = 'Y'
         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
         AND B2SP.BDRID = BDR.ID
         AND SPM.BRID = PARREVID;
         
           SELECT DMD.BPARENTBDREVISIONID INTO BPARREVID
     FROM SEDS_TA_DMBOEINGDOCUMENT DMD, SEDS_TA_BOEINGDOCUMENT BD
     WHERE BD.ID = DMD.BPARENTBDID
       AND DMD.BDOCUMENTID =BODCUMENTID;
         
         SELECT BDS.SUBMITDISPOSITIONID INTO PARDISPID FROM SEDS_TA_BDSUBMITTAL BDS WHERE BDS.BDRID=BPARREVID;
         
          SELECT BDS.VSUBMITTALTYPE INTO SUBMITTYPE
          FROM SEDS_TA_BDSUBMITTAL BDS
          where BDS.BDRID=BPARREVID;
          
         
  IF(ROUTESTS > 0 and (ATLASID = 0 OR ISROUTATSE = 0))THEN
       
       IF (SUBMITTYPE = 'P') THEN
        IF(PARDISPID =125 OR PARDISPID =126 OR PARDISPID=127 OR PARDISPID =128 OR PARDISPID =129) THEN  
         SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('P')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (2)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
               
          ELSIF(PARDISPID =130 OR PARDISPID =131 OR PARDISPID=132 OR PARDISPID =133 OR PARDISPID =134) THEN  
         SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('P')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (3)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
               
          ELSE
            SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('P')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (1)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
           
          END IF;
          END IF;

          IF (SUBMITTYPE = 'F') THEN
          
         IF(PARDISPID =120 OR PARDISPID =121 OR PARDISPID=122 OR PARDISPID =123 OR PARDISPID =124) THEN 
              SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('F')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (1)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
         
         ELSIF(PARDISPID =130 OR PARDISPID =131 OR PARDISPID=132 OR PARDISPID =133 OR PARDISPID =134) THEN 
         
         SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('F')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (3)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
      
         ELSE
         
         SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('F')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (2)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
         END IF;
        END IF;
         
                 
         
          IF (SUBMITTYPE = 'S') THEN
          
          IF(PARDISPID =125 OR PARDISPID =126 OR PARDISPID=127 OR PARDISPID =128 OR PARDISPID =129) THEN  
            SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('S')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (2)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
               
           ELSIF(PARDISPID =120 OR PARDISPID =121 OR PARDISPID=122 OR PARDISPID =123 OR PARDISPID =124) THEN 
              SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('S')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (1)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
               
           ELSE
          
              SELECT count(*) INTO OPENDATACOUNT
                FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                     SEDS_TA_BDR2SEDSPART          B2SP,
                     SEDS_VW_SUBMITPARTMODELLIST   SPM,
                     SEDS_TA_BDSUBMITTAL BDS
               WHERE BDR.ACTIVEIND = 'Y'         
               AND SPM.MASTERPARTID = B2SP.SEDSPARTID
               AND B2SP.BDRID = BDR.ID
               and BDS.VSUBMITTALTYPE in('S')
               AND SPM.BSID=BDS.ID
               AND BDR.DATAREQTTYPEID IN (3)
               AND SPM.BRID = BPARREVID and BDR.STATUSCODEID in (1,4,7,13,14);
            END IF;   
         END IF;
         
         
         
       IF(OPENDATACOUNT < 1 ) THEN
         /*SELECT min(lpad(PART.PARTNUMBER,30)) INTO PARTNUMBER
        FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
             SEDS_TA_BDR2SEDSPART          B2SP,
             SEDS_TA_SEDSPARTGROUP SPG,
             SEDS_TA_SUPPLIERPART PART,
             SEDS_VW_SUBMITPARTMODELLIST   SPM
       WHERE BDR.ACTIVEIND = 'Y'         
         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
         AND B2SP.BDRID = BDR.ID
         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
         AND SPG.SUPPLIERPARTID=PART.ID
         AND SPM.BRID = BPARREVID;*/
         
         IF (SUBMITTYPE = 'P') THEN
          
          IF(PARDISPID =130 OR PARDISPID =131 OR PARDISPID=132 OR PARDISPID =133 OR PARDISPID =134) THEN 
          
                       SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (3)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (3)
                         AND SPM.BRID = BPARREVID);
                         
           ELSIF(PARDISPID =125 OR PARDISPID =126 OR PARDISPID=127 OR PARDISPID =128 OR PARDISPID =129) THEN  
           
                 SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (2)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (2)
                         AND SPM.BRID = BPARREVID); 
         
       ELSE
          
         SELECT BDR.ID  INTO BOEINGDATAID
               FROM SEDS_TA_BDR2SEDSPART B2SP,
                    SEDS_TA_SEDSPART SP, 
                    SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                    SEDS_TA_SUPPLIERPART SPT,
                    SEDS_TA_SEDSPARTGROUP SPG
               WHERE B2SP.SEDSPARTID=SP.ID 
         AND BDR.ID=B2SP.BDRID 
         AND SPT.ID=SPG.SUPPLIERPARTID 
         AND BDR.DATAREQTTYPEID IN (1)
         AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
         AND SPT.PARTNUMBER IN(SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (1)
                         AND SPM.BRID = BPARREVID);
        END IF;
      END IF;
         
          IF (SUBMITTYPE = 'F') THEN
          
          IF(PARDISPID =120 OR PARDISPID =121 OR PARDISPID=122 OR PARDISPID =123 OR PARDISPID =124) THEN 
          
                       SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (1)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (1)
                         AND SPM.BRID = BPARREVID);
                         
           ELSIF(PARDISPID =130 OR PARDISPID =131 OR PARDISPID=132 OR PARDISPID =133 OR PARDISPID =134) THEN  
           
                 SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (3)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (3)
                         AND SPM.BRID = BPARREVID); 
         
       ELSE
          
         SELECT BDR.ID  INTO BOEINGDATAID
               FROM SEDS_TA_BDR2SEDSPART B2SP,
                    SEDS_TA_SEDSPART SP, 
                    SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                    SEDS_TA_SUPPLIERPART SPT,
                    SEDS_TA_SEDSPARTGROUP SPG
               WHERE B2SP.SEDSPARTID=SP.ID 
         AND BDR.ID=B2SP.BDRID 
         AND SPT.ID=SPG.SUPPLIERPARTID 
         AND BDR.DATAREQTTYPEID IN (2)
         AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
         AND SPT.PARTNUMBER IN(SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (2)
                         AND SPM.BRID = BPARREVID);
        END IF;
      END IF;
      
      
      IF (SUBMITTYPE = 'S') THEN
          
          IF(PARDISPID =120 OR PARDISPID =121 OR PARDISPID=122 OR PARDISPID =123 OR PARDISPID =124) THEN 
          
                       SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (1)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (1)
                         AND SPM.BRID = BPARREVID);
                         
           ELSIF(PARDISPID =125 OR PARDISPID =126 OR PARDISPID=127 OR PARDISPID =128 OR PARDISPID =129) THEN  
           
                 SELECT BDR.ID  INTO BOEINGDATAID
                         FROM SEDS_TA_BDR2SEDSPART B2SP,
                              SEDS_TA_SEDSPART SP, 
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_SUPPLIERPART SPT,
                              SEDS_TA_SEDSPARTGROUP SPG
                              WHERE B2SP.SEDSPARTID=SP.ID 
                          AND BDR.ID=B2SP.BDRID 
                          AND SPT.ID=SPG.SUPPLIERPARTID 
                          AND BDR.DATAREQTTYPEID IN (2)
                          AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
                          AND SPT.PARTNUMBER IN (SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (2)
                         AND SPM.BRID = BPARREVID); 
         
       ELSE
          
         SELECT BDR.ID  INTO BOEINGDATAID
               FROM SEDS_TA_BDR2SEDSPART B2SP,
                    SEDS_TA_SEDSPART SP, 
                    SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                    SEDS_TA_SUPPLIERPART SPT,
                    SEDS_TA_SEDSPARTGROUP SPG
               WHERE B2SP.SEDSPARTID=SP.ID 
         AND BDR.ID=B2SP.BDRID 
         AND SPT.ID=SPG.SUPPLIERPARTID 
         AND BDR.DATAREQTTYPEID IN (3)
         AND B2SP.SEDSPARTID=SPG.SEDSPARTID 
         AND SPT.PARTNUMBER IN(SELECT LTRIM(min(lpad(PART.PARTNUMBER,30)))
                         FROM SEDS_TA_BOEINGDATAREQUIREMENT BDR,
                              SEDS_TA_BDR2SEDSPART          B2SP,
                              SEDS_TA_SEDSPARTGROUP SPG,
                              SEDS_TA_SUPPLIERPART PART,
                              SEDS_VW_SUBMITPARTMODELLIST   SPM
                         WHERE BDR.ACTIVEIND = 'Y'         
                         AND SPM.MASTERPARTID = B2SP.SEDSPARTID
                         AND B2SP.BDRID = BDR.ID
                         AND B2SP.SEDSPARTID=SPG.SEDSPARTID
                         AND SPG.SUPPLIERPARTID=PART.ID
                         AND BDR.DATAREQTTYPEID IN (3)
                         AND SPM.BRID = BPARREVID);
        END IF;
      END IF;
          
         SELECT BDRE.REVISIONDATE INTO REVEDATE FROM SEDS_TA_BDREVISION BDRE,
         SEDS_TA_BDSUBMITTAL BDS
         WHERE BDS.BDRID = BDRE.ID AND BDS.ID = SUBMITALID;
         
         UPDATE SEDS_TA_BOEINGDATAREQUIREMENT BDR SET BDR.STATUSCODEID=1,
         BDR.REQUIREDDATE=(REVEDATE)+90 WHERE BDR.ID=BOEINGDATAID;
          COMMIT;
         
         INSERT INTO SEDS_TA_BDRDISCUSSION  VALUES(SEDS_SQ_BDRDISCUSSION_ID.NEXTVAL,BOEINGDATAID,'B','Re-Opened to ensure incorporation of outstanding TR(s)',LSID,SYSDATE,'N');
          COMMIT;
        
       END IF;
       END IF;
-- CODE END HERE FOR US 414969
         
         
    END IF;
  EXCEPTION
    WHEN OTHERS THEN
      SELECT SYSDATE INTO VERRORREC.DTSTAMP FROM DUAL;

      SELECT USER INTO VERRORREC.USERID FROM DUAL;

      VERRORREC.SQLERRORCODE := SQLCODE;
      VERRORREC.SQLMESSAGE   := SQLERRM;
      VERRORREC.FROMROUTINE  := 'SEDS_FN_SOPMUPDATE' + TEMP1;
      VERRORREC.NOTES        := 'CRITICAL: Failed while updating the SOPM Changes  :  SUBMITALID:' ||
                                SUBMITALID || ';BODCUMENTID:' ||
                                BODCUMENTID || ';CONDTION:' || CONDTION;
      SEDS_MANAGER.LOGDBERROR(VERRORREC);
  END;

  IF (CONDTION = 1) THEN

    IF (pardoc = 4) THEN

      UPDATE SEDS_TA_BDSUBMITTAL BSE
         SET BSE.PUBLISHACTIONID = 4
       WHERE BSE.ID = SUBMITALID;
      COMMIT;

      /** Added below update script for TFS bug 330975 - Santhosh **/
      UPDATE SEDS_TA_BDREVISION BRE
         SET BRE.REVISIONDATE  = (SELECT S.HREVISIONDATE
                                    FROM SEDS_TA_BDSUBMITTAL S
                                   WHERE S.ID = SUBMITALID
                                     AND S.BDRID = BRE.ID
                                     AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL),
             BRE.LASTREVISEDBY = (SELECT S.Lastrevisedby
                                    FROM SEDS_TA_BDSUBMITTAL S
                                   WHERE S.ID = SUBMITALID
                                     AND S.BDRID = BRE.ID
                                     AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL)

       WHERE BRE.ID =
             ((SELECT S.BDRID
                 FROM SEDS_TA_BDSUBMITTAL S
                WHERE S.ID = SUBMITALID
                  AND S.BDRID = BRE.ID
                  AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL));
      COMMIT;

      TARGET := 1;

    ELSE

      SELECT BDS.SUBMITDISPOSITIONID
        INTO DISPID
        FROM SEDS_TA_BDSUBMITTAL BDS
       WHERE BDS.ID = SUBMITALID;

      IF (DISPID = 157) THEN
        TARGET := 1;
      ELSIF(DISPID = 200)THEN
            TARGET := 1;
       ELSE
        TARGET := 2;
      END IF;

    END IF;

    IF (pardoc != 4 AND ROUTESTS = 0) THEN

      UPDATE SEDS_TA_BDSUBMITTAL BSE
         SET BSE.PUBLISHACTIONID = 4
       WHERE BSE.ID = SUBMITALID;
      COMMIT;

      /** Added below update script for TFS bug 330975 - Santhosh **/
      UPDATE SEDS_TA_BDREVISION BRE
         SET BRE.REVISIONDATE  = (SELECT S.HREVISIONDATE
                                    FROM SEDS_TA_BDSUBMITTAL S
                                   WHERE S.ID = SUBMITALID
                                     AND S.BDRID = BRE.ID
                                     AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL),
             BRE.LASTREVISEDBY = (SELECT S.Lastrevisedby
                                    FROM SEDS_TA_BDSUBMITTAL S
                                   WHERE S.ID = SUBMITALID
                                     AND S.BDRID = BRE.ID
                                     AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL)

       WHERE BRE.ID =
             ((SELECT S.BDRID
                 FROM SEDS_TA_BDSUBMITTAL S
                WHERE S.ID = SUBMITALID
                  AND S.BDRID = BRE.ID
                  AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL));
      COMMIT;

    END IF;
  
--  414969  
  IF(ROUTESTS > 0 and ATLASID > 0 AND ISROUTATSE >0)THEN 
       TARGET :=3;

   ELSIF (ROUTESTS > 0 and (ATLASID = 0 OR ISROUTATSE = 0))THEN
       TARGET := 2;
  END IF;
  

    -- Copy values from Submittal table to Volume table
    TEMP1 := SEDS_FN_SBUPDATE(SUBMITALID, VOLID, 1);

  END IF;

  IF (DOCTYPE = 4 AND CONDTION = 2) THEN
    UPDATE SEDS_TA_BOEINGDATAREQUIREMENT BDR1
       SET BDR1.STATUSCODEID        = 2,
           BDR1.LASTREVISEDBY       = LSID,
           BDR1.LASTREVISEDSTAMP    = SYSDATE,
           BDR1.RECEIPTDATE         = SYSDATE,
           BDR1.STATUSMODIFIEDSTAMP = SYSDATE,
           BDR1.STATUSMODIFIEDBY    = LSID
     WHERE BDR1.ID IN (SELECT BDR.ID
                         FROM SEDS_TA_DMBOEINGDOCUMENT      DMB,
                              SEDS_TA_BDR2BD                B2B,
                              SEDS_TA_BOEINGDATAREQUIREMENT BDR
                        WHERE DMB.BDOCUMENTTYPEID = 4
                          AND DMB.BDOCUMENTID = BODCUMENTID
                          AND DMB.BSUBMITTALID = SUBMITALID
                          AND B2B.BDSUBMITTALID = DMB.BSUBMITTALID
                          AND B2B.BDRID = BDR.ID
                          AND BDR.ACTIVEIND = 'Y'
                          AND BDR.STATUSCODEID IN (1, 4, 7));

    COMMIT;

    UPDATE SEDS_TA_BDSUBMITTAL BDS
       SET BDS.SUBMITDISPOSITIONID = 154,
           BDS.PUBLISHACTIONID     = 4,
           BDS.COMPLETIONDATE      = SYSDATE,
           BDS.LASTREVISEDSTAMP    = SYSDATE
     WHERE BDS.ID = SUBMITALID;

    COMMIT;

    /** Added below update script for TFS bug 330975 - Santhosh **/
    UPDATE SEDS_TA_BDREVISION BRE
       SET BRE.REVISIONDATE  = (SELECT S.HREVISIONDATE
                                  FROM SEDS_TA_BDSUBMITTAL S
                                 WHERE S.ID = SUBMITALID
                                   AND S.BDRID = BRE.ID
                                   AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL),
           BRE.LASTREVISEDBY = (SELECT S.Lastrevisedby
                                  FROM SEDS_TA_BDSUBMITTAL S
                                 WHERE S.ID = SUBMITALID
                                   AND S.BDRID = BRE.ID
                                   AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL)

     WHERE BRE.ID =
           ((SELECT S.BDRID
               FROM SEDS_TA_BDSUBMITTAL S
              WHERE S.ID = SUBMITALID
                AND S.BDRID = BRE.ID
                AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL));
    COMMIT;

    /** Added below line for bugzilla bug 31773 - Santhosh **/

    TEMP1 := SEDS_FN_SBUPDATE(SUBMITALID, VOLID, 1);

  ELSIF (DOCTYPE = 9 AND CONDTION = 3) THEN

    IF STSREJECTCHECK IN (159, 160) THEN
      IF (ISPUBLISHED = 3) THEN
        UPDATE SEDS_TA_BDSUBMITTAL BSE
           SET BSE.PUBLISHACTIONID = 4
         WHERE BSE.ID = SUBMITALID;

        /** Added below update script for TFS bug 330975 - Santhosh **/
        UPDATE SEDS_TA_BDREVISION BRE
           SET BRE.REVISIONDATE  = (SELECT S.HREVISIONDATE
                                      FROM SEDS_TA_BDSUBMITTAL S
                                     WHERE S.ID = SUBMITALID
                                       AND S.BDRID = BRE.ID
                                       AND S.HREVISIONLEVEL =
                                           BRE.REVISIONLEVEL),
               BRE.LASTREVISEDBY = (SELECT S.Lastrevisedby
                                      FROM SEDS_TA_BDSUBMITTAL S
                                     WHERE S.ID = SUBMITALID
                                       AND S.BDRID = BRE.ID
                                       AND S.HREVISIONLEVEL =
                                           BRE.REVISIONLEVEL)

         WHERE BRE.ID =
               ((SELECT S.BDRID
                   FROM SEDS_TA_BDSUBMITTAL S
                  WHERE S.ID = SUBMITALID
                    AND S.BDRID = BRE.ID
                    AND S.HREVISIONLEVEL = BRE.REVISIONLEVEL));
        COMMIT;
      END IF;

    END IF;

    --CMM Workflow - R7.vsd TR SDD Reject check
    IF STSREJECTCHECK = 157 THEN
      TARGET := 1;
      IF (ISPUBLISHED = 3) THEN
        UPDATE SEDS_TA_BDSUBMITTAL BSE
           SET BSE.PUBLISHACTIONID  = 16,
               BSE.LASTREVISEDBY    = LSID,
               BSE.LASTREVISEDSTAMP = SYSDATE
         WHERE BSE.ID = SUBMITALID;
      END IF;
    ELSIF STSREJECTCHECK = 163 THEN
    TARGET := 1;
      IF (ISPUBLISHED = 3) THEN
        UPDATE SEDS_TA_BDSUBMITTAL BSE
           SET BSE.PUBLISHACTIONID  = 18,
               BSE.LASTREVISEDBY    = LSID,
               BSE.LASTREVISEDSTAMP = SYSDATE
         WHERE BSE.ID = SUBMITALID;
      END IF;
    END IF;
    if STSREJECTCHECK = 158 then

    ---aDD PARENT CMM/IPC ROUTE2STS CHECK
     UPDATE SEDS_TA_BDSUBMITTAL BSE
           SET BSE.PUBLISHACTIONID  = 4,
               BSE.LASTREVISEDBY    = LSID,
               BSE.LASTREVISEDSTAMP = SYSDATE
         WHERE BSE.ID = SUBMITALID;
  END IF;

    end if;

  COMMIT;

  RETURN TARGET;
EXCEPTION
  WHEN OTHERS THEN
    SELECT SYSDATE INTO VERRORREC.DTSTAMP FROM DUAL;

    SELECT USER INTO VERRORREC.USERID FROM DUAL;

    VERRORREC.SQLERRORCODE := SQLCODE;
    VERRORREC.SQLMESSAGE   := SQLERRM;
    VERRORREC.FROMROUTINE  := 'SEDS_FN_SOPMUPDATE' + TEMP1;
    VERRORREC.NOTES        := 'CRITICAL: Failed while updating the SOPM Changes  :  SUBMITALID:' ||
                              SUBMITALID || ';BODCUMENTID:' || BODCUMENTID ||
                              ';CONDTION:' || CONDTION;
    SEDS_MANAGER.LOGDBERROR(VERRORREC);
    RETURN 1;
End SEDS_FN_SOPMUPDATE;
