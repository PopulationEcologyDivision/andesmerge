-- reset
--DELETE from gsINF WHERE mission = 'CAR2022102';
--DELETE from gsCAT WHERE mission = 'CAR2022102';
--DELETE from gsDET WHERE mission = 'CAR2022102';
--DELETE from gs_LV1_Observations WHERE mission = 'CAR2022102';

-- this just has all the SQL that would be executed from the button

-- *****  GSWARPOUT  *********

DROP TABLE backup_gswarpout;

CREATE TABLE backup_gswarpout AS SELECT * FROM gswarpout WHERE mission = 'CAR2022102';

DELETE gswarpout WHERE mission = 'CAR2022102';

INSERT INTO gswarpout (mission,setno,warpout)
			SELECT  mission,setno,warpout FROM ese_sets WHERE mission = 'CAR2022102';


commit;
DROP TABLE BACKUP_GSINF;

commit; 
CREATE  TABLE BACKUP_GSINF AS SELECT * FROM GSINF WHERE mission = 'CAR2022102';

commit; 
DELETE GSINF WHERE mission = 'CAR2022102';

commit;	
	INSERT INTO GSINF
	 (mission,setno,sdate,time,etime,strat,slat,slong,elat,elong,area,dur,dist,howd,speed,hows,dmin,dmax,wind,force,curnt,
	 type,gear,aux,depth,remarks,start_depth,end_depth,surface_temperature,bottom_temperature,bottom_salinity,station)
	SELECT  mission,setno,
		TO_DATE(start_date||start_time,'DDMMYYYYHH24MI'),start_time,TO_DATE(end_date||end_time,'DDMMYYYYHH24MI'), 
		strat,slat,slong,elat,elong,area,
		1440*(TO_DATE(end_date||end_time,'DDMMYYYYHH24MI')-TO_DATE(start_date||start_time,'DDMMYYYYHH24MI')),
		Dist,howd,speed,hows,dmin,dmax,wind,force,curnt,experiment_type_code,gear,aux,NULL,SUBSTR(note,1,250),
		start_depth,end_depth,surface_temperature,bottom_temperature,bottom_salinity,station
	FROM  ESE_SETS
	WHERE mission = 'CAR2022102';


--  *****  GSCAT  *********************	

commit;  
DROP TABLE BACKUP_GSCAT;

commit; 
CREATE TABLE BACKUP_GSCAT AS SELECT * FROM GSCAT WHERE mission = 'CAR2022102';

commit; 
DELETE GSCAT WHERE mission = 'CAR2022102';

commit;
DROP TABLE TEMP_GSCAT;

commit; 
CREATE  TABLE TEMP_GSCAT AS SELECT * FROM  GSCAT WHERE  mission = 'XXXXXXXXXX';

commit; 

INSERT INTO TEMP_GSCAT
		(mission,setno,spec,size_class,market,sampwgt,totwgt,totno,calwt,remarks)
	SELECT  mission,setno,spec,size_class,null,null,null,null,null,note
FROM ESE_CATCHES  WHERE mission = 'CAR2022102';

commit; 

UPDATE  TEMP_GSCAT SET  TEMP_GSCAT.SAMPWGT =
	(SELECT  	SUM(ese_baskets.basket_weight)
	FROM  		ESE_BASKETS
	WHERE  		temp_gscat.mission = ese_baskets.mission AND
				temp_gscat.setno = ese_baskets.setno AND 
				temp_gscat.spec  = ese_baskets.spec AND 
				temp_gscat.size_class = ese_baskets.size_class AND 
				ese_baskets.sampled ='Y');

commit; 
DROP TABLE TEMP_GSCAT2;

commit; 

CREATE  TABLE TEMP_GSCAT2 AS 
	SELECT mission,setno,spec,size_class,SUM(basket_weight) sumw, AVG(basket_weight) avgw
	FROM  ESE_BASKETS
	WHERE mission = 'CAR2022102'
	GROUP BY  mission,setno,spec,size_class;

commit; 
ALTER TABLE TEMP_GSCAT2 ADD (unweighed_baskets number);

commit; 

UPDATE TEMP_GSCAT2 SET unweighed_baskets =
	(SELECT  ESE_CATCHES.unweighed_baskets
	FROM  ESE_CATCHES
	WHERE TEMP_GSCAT2.mission = ESE_CATCHES.mission AND
		temp_gscat2.setno = ese_catches.setno AND 
		temp_gscat2.spec  = ese_catches.spec AND 
		temp_gscat2.size_class = ese_catches.size_class);

commit; 
UPDATE TEMP_GSCAT2
	   SET unweighed_baskets = 0
	   WHERE unweighed_baskets is null;

commit; 

UPDATE TEMP_GSCAT
	SET totwgt = (SELECT  sumw+(avgw*unweighed_baskets)
	FROM TEMP_GSCAT2
	WHERE TEMP_GSCAT2.mission = temp_gscat.mission AND
		temp_gscat2.setno = temp_gscat.setno AND  
		temp_gscat2.spec  = temp_gscat.spec AND
		temp_gscat2.size_class = temp_gscat.size_class);
	
commit; 
DROP TABLE TEMP_GSDET;

commit; 

CREATE TABLE TEMP_GSDET AS 
	SELECT mission, setno,spec,size_class,COUNT(*) cnt
	FROM ESE_SPECIMENS
	WHERE mission = 'CAR2022102'  
	GROUP BY  mission,setno,spec,size_class; 
--	IF NOT Form_Success THEN
--		Message ('create TEMP_gsdet. Failed. '||to_char(DBMS_ERROR_CODE)||' '||DBMS_ERROR_TEXT);
--	END IF;

commit; 

UPDATE TEMP_GSCAT SET TOTNO = 
	(SELECT  cnt
	FROM  TEMP_GSDET
	WHERE temp_gscat.mission = temp_gsdet.mission AND 
			temp_gscat.setno = temp_gsdet.setno AND 
			temp_gscat.spec  = temp_gsdet.spec AND 
			temp_gscat.size_class = temp_gsdet.size_class);

commit; 
UPDATE  TEMP_GSCAT SET  totno = totno * totwgt/sampwgt WHERE  sampwgt <> 0 AND  totwgt <> 0;
--  IF NOT Form_Success THEN
--		Message ('UPDATE  TEMP_GSCAT. Failed. '||to_char(DBMS_ERROR_CODE)||' '||DBMS_ERROR_TEXT);
--	END IF;

commit; 

UPDATE  TEMP_GSCAT SET  TOTNO = 
	(SELECT  ese_catches.number_caught
	FROM  ESE_CATCHES
	WHERE temp_gscat.mission = ese_catches.mission AND 
		temp_gscat.setno = ese_catches.setno AND 
		temp_gscat.spec = ese_catches.spec AND 
		temp_gscat.size_class = ese_catches.size_class AND 
		ese_catches.number_caught <> 0 AND 
		ese_catches.number_caught is not null)
WHERE  temp_gscat.totno is null;

commit; 
UPDATE  TEMP_GSCAT SET  SAMPWGT = 0 WHERE  SAMPWGT IS NULL;

commit; 
UPDATE  TEMP_GSCAT SET  TOTWGT  = 0 WHERE  TOTWGT  IS NULL;

commit; 
UPDATE  TEMP_GSCAT SET  TOTNO = 0 WHERE  TOTNO IS NULL;

commit; 
UPDATE  TEMP_GSCAT SET  CALWT = 0;

-- v8.4 add length_type, length_units, weight_type, weight_units updates

commit; 
UPDATE  TEMP_GSCAT set LENGTH_TYPE = 
	(	select e.length_type from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	)
	where exists
	(	select e.length_type from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	);

commit; 
UPDATE  TEMP_GSCAT set LENGTH_UNITS = 
	(	select e.length_units from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	)
	where exists
	(	select e.length_units from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	);

commit; 
UPDATE  TEMP_GSCAT set WEIGHT_TYPE = 
	(	select e.weight_type from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	)
	where exists
	(	select e.weight_type from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	);

commit; 
UPDATE  TEMP_GSCAT set WEIGHT_UNITS = 
	(	select e.weight_units from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	)
	where exists
	(	select e.weight_units from ese_sampreq_species e 
		where e.mission = 'CAR2022102' and TEMP_GSCAT.spec = e.species_code
	);

-- insert from temp gscat mission table to prod gscat
commit; 
INSERT INTO GSCAT
	(mission,setno,spec,size_class,market,sampwgt,totwgt,totno,calwt,remarks,
		length_type, length_units, weight_type, weight_units)
	SELECT  mission,setno,spec,size_class,null,sampwgt,totwgt,totno,calwt,remarks,
		length_type, length_units, weight_type, weight_units
	FROM TEMP_GSCAT;


--  ***********  GSDET  **************************

commit;   
DROP TABLE BACKUP_GSDET;

commit; 
CREATE TABLE BACKUP_GSDET AS  SELECT * FROM  GSDET WHERE mission = 'CAR2022102';

commit;  
DELETE GSDET WHERE mission = 'CAR2022102';

commit;

INSERT INTO  GSDET
	(mission,setno,spec,size_class,fshno,flen,fsex,fmat,fwt,agmat,remarks,nann,edge,chkmrk,age,ager,clen, specimen_id)
SELECT mission,setno,spec,size_class,fish_number,length,sex,
	maturity,weight,age_material_type,comments,NULL,NULL,NULL,NULL,NULL,1, SPECIMEN_ID
FROM  ESE_DETAIL where mission = 'CAR2022102';


-- ******  GS_LV1_OBSERVATIONS  ******************************	

-- this will be added to app but not deployed yet
--commit;  
--DELETE GS_LV1_OBSERVATIONS WHERE mission = 'CAR2022102';

commit;
INSERT INTO  GS_LV1_OBSERVATIONS
	select * from ese_lv1_observations 
	where lv1_observation not in ('Length','Weight','Sex','Maturity','Fish Number', 'Age Material Type', 'Comments') and
	mission = 'CAR2022102';

commit;    
INSERT INTO  GS_LV2_OBSERVATIONS
	select * from ese_lv2_observations 
	where mission = 'CAR2022102';

commit;  
INSERT INTO  GS_LV3_OBSERVATIONS
	select * from ese_lv3_observations 
	where mission = 'CAR2022102';

commit;  
INSERT INTO  GS_LV4_OBSERVATIONS
	select * from ese_lv4_observations 
	where mission = 'CAR2022102';

-- ********** clean up, remark out during Dev if you want to see the TEMP tables

--commit; 
--DROP TABLE TEMP_GSCAT';

--commit; 
--DROP TABLE TEMP_GSCAT2';

--commit; 
--DROP TABLE TEMP_GSDET';

