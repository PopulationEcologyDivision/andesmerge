


--Change depth from meters to fathoms to match what is in the database.

Update ese_sets Set dmin=dmin/1.8288 where mission = 'CAR2021240';
Update ese_sets Set dmax=dmax/1.8288 where mission = 'CAR2021240';
Update ese_sets Set warpout=warpout/1.8288 where mission = 'CAR2021240';
Update ese_sets Set start_depth=start_depth/1.8288 where mission = 'CAR2021240';
Update ese_sets Set end_depth=end_depth/1.8288 where mission = 'CAR2021240';

--DONE


-------------------------------------------------------------------------------
-- 1) Check the maximum and minimum values from the sets table.
-- Print the results from notepad and examine for irregularities,
-- checking the paperwork for validity.

select  min(to_date(start_date,'DDMMYYYY')) min_start_date, max(to_date(start_date,'DDMMYYYY')) max_start_date,
        min(to_date(end_date,'DDMMYYYY')) min_end_date, max(to_date(end_date,'DDMMYYYY')) max_end_date,
        min(strat) min_strat, max(strat) max_strat,
        min(slat) min_slat, max(slat) max_slat,
        min(elat) min_elat, max(elat) max_elat,
        min(slong) min_slong, max(slong) max_slong,
        min(elong) min_elong, max(elong) max_elong,
        min(dist) min_dist, max(dist)max_dist,
        min(speed) min_speed, max(speed) max_speed,
        min(dmin) min_dmin,max(dmin) max_dmin,
        min(dmax)min_dmax,max(dmax) max_dmax
from ese_sets where mission = 'CAR2021240' and experiment_type_code=1;

--DONE

--IF NEED TO UPDATE 
--update ESE_SETS
--set EXPERIMENT_TYPE_CODE=8 WHERE MISSION='VEN2018002' AND SETNO=54 AND STRAT='5Z2'
--AND EXPERIMENT_TYPE_CODE=1;

--UPDATE GSINF
--SET TYPE=8 WHERE MISSION='VEN2018002' AND SETNO=54 AND STRAT='5Z2' AND TYPE=1;


-- FIX strat should be uppercase (ONLY FOR GB)
update ese_sets set strat = '5Z9' where strat = '5z9'; -- 1 row
update ese_sets set strat = '5Z8' where strat = '5z8';
update ese_sets set strat = '5Z7' where strat = '5z7';
update ese_sets set strat = '5Z6' where strat = '5z6';
update ese_sets set strat = '5Z5' where strat = '5z5';
update ese_sets set strat = '5Z4' where strat = '5z4';
update ese_sets set strat = '5Z3' where strat = '5z3';
update ese_sets set strat = '5Z2' where strat = '5z2';
update ese_sets set strat = '5Z1' where strat = '5z1';

--DONE

-------------------------------------------------------------------------------
-- 2) Check for null values in the sets table.
--


Select mission,setno,experiment_type_code,howd from ese_sets where mission = 'CAR2021240' and howd is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,speed from ese_sets where mission = 'CAR2021240' and speed is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,hows from ese_sets where mission = 'CAR2021240' and hows is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,hows from ese_sets where mission = 'CAR2021240' and wind is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,force from ese_sets where mission = 'CAR2021240' and force is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,curnt from ese_sets where mission = 'CAR2021240' and curnt is null and experiment_type_code=1;
Select mission,setno,experiment_type_code from ese_sets where mission = 'CAR2021240' and experiment_type_code is null;
Select mission,setno,experiment_type_code,gear from ese_sets where mission = 'CAR2021240' and gear is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,aux from ese_sets where mission = 'CAR2021240' and aux is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,warpout from ese_sets where mission = 'CAR2021240' and warpout is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,start_date from ese_sets where mission = 'CAR2021240' and start_date is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,end_date from ese_sets where mission = 'CAR2021240' and end_date is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,strat from ese_sets where mission = 'CAR2021240' and strat is null;
Select mission,setno,experiment_type_code,slat from ese_sets where mission = 'CAR2021240' and slat is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,elat from ese_sets where mission = 'CAR2021240' and elat is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,slong from ese_sets where mission = 'CAR2021240' and slong is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,elong from ese_sets where mission = 'CAR2021240' and elong is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,area from ese_sets where mission = 'CAR2021240' and area is null;
Select mission,setno,experiment_type_code,dist from ese_sets where mission = 'CAR2021240' and dist is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,dmin from ese_sets where mission = 'CAR2021240' and dmin is null and experiment_type_code=1;
Select mission,setno,experiment_type_code,dmax from ese_sets where mission = 'CAR2021240' and dmax is null and experiment_type_code=1;

--Issue with a blank set created while at sea.  Kept the blank in the DB so to avoid possible future confusion 
--some issues with blank gear aux code.  Nothing currently exists for NEST gear
--dmin and dmax blanks updated manually from start and end depths
--IF NEEDED

--DELETE ESE_SETS where mission='CAR2021240' and setno=9 and experiment_type_code is null;
--UPDATE ESE_SETS SET CURNT=6 WHERE MISSION='CAR2021240' AND SETNO = 22 AND CURNT IS NULL;


--DONE


-------------------------------------------------------------------------------
-- 3) This script checks the species by set number in the catches  
-- table and compares it to the species by set number in the baskets table 
-- and lists the missing data from the baskets table by set number and species
-- next it checks the species by set number in the baskets table and
-- compares it to the species by set number in the detail table and
-- lists the missing data from the detail table
-- 
select setno, spec, size_class from ese_catches where mission = 'CAR2021240'   
minus
select setno, spec, size_class from ese_baskets where mission = 'CAR2021240' 
order by setno, spec, size_class;
--13 issues
--delete ese_catches where mission='CAR2021240' and spec = 620 and setno = 2;
--delete ese_catches where mission='CAR2021240' and spec = 8300 and setno = 10;
--delete ese_catches where mission='CAR2021240' and spec = 68 and setno = 18;
--delete ese_catches where mission='CAR2021240' and spec = 12 and setno = 20 and size_class=2;
--delete ese_catches where mission='CAR2021240' and spec = 9999;
--insert into ESE_BASKETS values ('CAR2021240', 46, 6600, 1, 0.005, 'N');
--delete ese_catches where mission='CAR2021240' and spec = 6110 and setno = 91;


--DONE

--Other possible fixes:
--UPDATE ESE_CATCHES
--SET SIZE_CLASS=1 WHERE mission='NED2017020' and spec = 11 and setno = 241 AND SIZE_CLASS=3;

--UPDATE ESE_BASKETS
--SET SIZE_CLASS=1 WHERE mission='NED2017020' and spec = 11 and setno = 241 AND SIZE_CLASS=3;

--UPDATE ESE_LV1_OBSERVATIONS
--SET SIZE_CLASS=1 WHERE mission='NED2017020' and spec = 11 and setno = 241 AND SIZE_CLASS=3;

--UPDATE ESE_SPECIMENS
--SET SIZE_CLASS=1 WHERE mission='NED2017020' and spec = 11 and setno = 241 AND SIZE_CLASS=3;




select setno, spec, size_class from ese_baskets where sampled = 'Y' and mission = 'CAR2021240'
minus
select setno, spec, size_class from ese_specimens where mission = 'CAR2021240'
order by setno, spec, size_class;

--1 ISSUE

--UPDATE ESE_BASKETS
--SET SAMPLED ='N' WHERE MISSION='CAR2021240'AND SETNO=64 AND SPEC=4508 AND BASKET_WEIGHT=0.0035 AND SAMPLED='Y';


--OTHER OPTIONS:

--UPDATE ESE_BASKETS
--SET SAMPLED ='N' WHERE MISSION='CAR2021240'AND SETNO=151 AND SPEC=2526 AND BASKET_WEIGHT=0.002 AND SAMPLED='Y';

--UPDATE ESE_BASKETS
--SET SAMPLED ='N' WHERE MISSION='CAR2021240'AND SETNO=94 AND SPEC=4500 AND BASKET_WEIGHT=0.0003 AND SAMPLED='Y';

--no detail entered for Silver Hake??? ugh
--UPDATE ESE_BASKETS
--SET SAMPLED ='N' WHERE MISSION='CAR2021240'AND SETNO=80 AND SPEC=14 AND BASKET_WEIGHT=5.04 AND SAMPLED='Y';

--CHECK YELLOWTAIL FLOUNDER ON SET 82.  No detail entered but weight same as Winter Flounder.  Likely a misidentification during catch entry and was not removed. Delete YT Flounder.
--delete ESE_CATCHES where MISSION='CAR2021240'AND SETNO=82 AND SPEC=42 AND SIZE_CLASS=1;
--delete ESE_BASKETS where MISSION='CAR2021240'AND SETNO=82 AND SPEC=42 AND BASKET_WEIGHT=0.274 AND SAMPLED='Y';

--Other possible fixes:

--UPDATE ESE_LV1_OBSERVATIONS
--SET SPEC=4511 WHERE MISSION='NED2017020' AND SETNO=24 AND SPEC=60 AND SPECIMEN_ID > 7264;
--UPDATE ESE_LV1_OBSERVATIONS
--SET DATA_DESC='Centimeters' where MISSION='NED2017020' AND SETNO=24 AND spec=4511 and lv1_observation='Length'
--and data_desc='Millimeters';
--UPDATE ESE_SPECIMENS
--SET SPEC=4511 WHERE MISSION='NED2017020' AND SETNO=24 AND SPEC=60 AND SPECIMEN_ID > 7264;

--ISSUE: MYCTOPHID(150) SIZE CLASS 2 DOES NOT HAVE ANY DETAIL ENTERED SO CHANGED TO 'N'
--update ESE_BASKETS
--SET SAMPLED='N' WHERE mission='NED2017020' and spec = 150 and setno = 179 AND SIZE_CLASS=2;

--Issue: Lobster entered as 2 size classes but detail may have all been entered under size class 1 (Set 46). It appears that the not intact lobsters
--may have been put in as size class 2 but not sure.  Making the size class 2 basket 'N'
--update ESE_BASKETS
--SET SAMPLED='N' WHERE mission='NED2017020' and spec = 2550 and setno = 46 AND SIZE_CLASS=2;



--species in ESE_baskets not in ESE_specimens, need to be deleted or updated
--This may have been entered by error and not removed

--delete ESE_BASKETS where mission='TEL2017002' and SETNO=2 and SPEC=3 and
--SIZE_CLASS=1 and BASKET_WEIGHT=8600 AND SAMPLED='Y';

--DONE

-------------------------------------------------------------------------------
-- 4) Check for outliers in the realm of basket weights.  Remember
-- that some species will have large basket weight entries,
-- especially for shrimp which are generally going to be calculated 
-- from subsampling the total catch.
--
-- Weights greater than 40, less than .001 or null should be looked 
-- into
--
select * from ese_baskets where basket_weight > 40 and mission = 'CAR2021240'
order by setno, spec, size_class;
--SEVERAL BASKETS EXCEEDED 40. USED RECTANGLE BASKETS ON CARTIER AND THEY HOLD MORE, PLUS USED THE WEIGHING BIN FOR LARGE CATCHES

--if needed, can use something like below
--update ESE_BASKETS
--set BASKET_WEIGHT=40 where Mission='NED2014018' and SETNO=156 and SPEC=23 and
--BASKET_WEIGHT=1096.92 and SIZE_CLASS=1;
--insert into ESE_BASKETS values ('NED2014018', 156, 23, 1, 1057.11, 'N');

--DONE

select * from ese_baskets where basket_weight < .001 and mission = 'CAR2021240'
order by setno, spec, size_class;
--SEVERAL SPECIES WEIGHED LESS THAN A GRAM SO weight entered as half of a gram
--fixed one entry where 'Sampled' was not indicated as Null for a basket
--UPDATE ESE_BASKETS
--SET SAMPLED='N' WHERE mission='CAR2021240' and setno=144 and spec=2893
--and size_class=1 and sampled IS NULL;
--UPDATE ESE_BASKETS
--SET SAMPLED='N' WHERE mission='CAR2021240' and setno=144 and spec in (6113,6500,8100)
--and size_class=1 and sampled IS NULL;


--OPTION SCRIPTS

--delete ESE_BASKETS where basket_weight=0 and mission='NED2017020' and setno=85 and spec IN (2990,4330)
--and size_class=1 and sampled='N';

--UPDATE ESE_BASKETS
--SET SAMPLED='N' WHERE mission='NED2017020' and setno=45 and spec=1901
--and size_class=1 and sampled IS NULL;

--if needed, can use something like below
--update ESE_BASKETS
--set basket_weight=0.002 where mission='NED2014018' and setno=160 and spec=2313
--and size_class=1 and basket_weight=0 and sampled='N';
--OR
--delete ESE_BASKETS where basket_weight=0 and mission='NED2014018' and setno=183 and spec=8347
--and size_class=1 and basket_weight=0 and sampled='N';

--DONE

select * from ese_baskets where basket_weight is null and mission = 'CAR2021240'
order by setno, spec, size_class;

--DONE


-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 6) Check for unusual lengths and weights for particular species by 
-- examining the minimum and maximum values for each species
-- encountered. After fixing the outliers, rerun this routine a 
-- few times.  Often there are more than one fish which stretch 
-- the real imagination.
--
select distinct(spec), min(to_number(data_value)) min_length, max(to_number(data_value)) max_length
from ese_lv1_observations 
where lv1_observation = 'Length' and mission = 'CAR2021240'
group by spec
order by spec;

--several issues where length appears to have been entered incorrectly (less than 3cm - unlikely)
--have to either delete or change to the most likely length
--made some changes individually, then made a group deletion below

--delete ese_lv1_observations where mission='CAR2021240' and setno=8 and spec=640 and specimen_id=3084;
--delete ese_specimens where mission='CAR2021240' and setno=8 and spec=640 and specimen_id=3084;
--delete ese_lv1_observations where mission='CAR2021240' and setno=88 and spec=11 and specimen_id=33119;
--delete ese_specimens where mission='CAR2021240' and setno=88 and spec=11 and specimen_id=33119;
--update ese_lv1_observations 
--set data_value=10 where mission='CAR2021240' and setno=88 and spec=11 and LV1_OBSERVATION='Length' and specimen_id = 33212;
--delete ese_lv1_observations where mission='CAR2021240' and setno=5 and spec=11 and specimen_id in (1032,1050,990,1027);
--delete ese_specimens where mission='CAR2021240' and setno=5 and spec=11 and specimen_id in (1032,1050,990,1027);

--delete ese_lv1_observations where mission='CAR2021240' and setno=19 and spec=61 and specimen_id = 7837;
--delete ese_specimens where mission='CAR2021240' and setno=19 and spec=61 and specimen_id = 7837;
--delete ese_lv1_observations where mission='CAR2021240' and setno=20 and spec=300 and specimen_id = 7996;
--delete ese_specimens where mission='CAR2021240' and setno=20 and spec=300 and specimen_id = 7996;
--delete ese_lv1_observations where mission='CAR2021240' and setno=25 and spec=62 and specimen_id in (10451,10461);
--delete ese_specimens where mission='CAR2021240' and setno=25 and spec=62 and specimen_id in (10451,10461);
--delete ese_lv1_observations where mission='CAR2021240' and setno=33 and spec=13 and specimen_id in (13762,13793);
--delete ese_specimens where mission='CAR2021240' and setno=33 and spec=13 and specimen_id in (13762,13793);
--delete ese_lv1_observations where mission='CAR2021240' and setno=35 and spec=14 and specimen_id in (14413,14421,14503);
--delete ese_specimens where mission='CAR2021240' and setno=35 and spec=14 and specimen_id in (14413,14421,14503);

--delete ese_lv1_observations where mission='CAR2021240' and specimen_id in (20261,20336,10451,10461,14421,7837,7996,13762,
--13793,23933,30729,29828,29906,30191,30202,29604);
--delete ese_specimens where mission='CAR2021240' and specimen_id in (20261,20336,10451,10461,14421,7837,7996,13762,
--13793,23933,30729,29828,29906,30191,30202,29604);


--Other options:

--adding 0.5 and convert to mm
--update ese_lv1_observations 
--set data_value=245 where mission='CAR2021240' and setno=61 and spec=60 and LV1_OBSERVATION='Length' and specimen_id = 16331;
--update ese_lv1_observations 
--set data_value=255 where mission='CAR2021240' and setno=61 and spec=60 and LV1_OBSERVATION='Length' and specimen_id = 16328;

--UPDATE ESE_LV1_OBSERVATIONS
--SET DATA_VALUE=(DATA_VALUE*10) WHERE MISSION='CAR2021240' AND SETNO=36 AND SPEC=60 AND LV1_OBSERVATION='Length';

--Other possible fixes:

--update ese_lv1_observations 
--set data_value=15 where mission='CAR2021240' and setno=63 and spec=4511 and LV1_OBSERVATION='Length' and specimen_id=20982;

--update ESE_CATCHES
--set spec=330 where mission='TEL2017002' and setno=26 and spec=331;

--update ESE_BASKETS
--set spec=330 where mission='TEL2017002' and setno=26 and spec=331 and basket_weight=0.005;

--update ESE_LV1_OBSERVATIONS
--set spec=330 where mission='TEL2017002' and setno=26 and spec=331 and specimen_id=7916;

--update ESE_SPECIMENS
--set spec=330 where mission='TEL2017002' and setno=26 and spec=331 and specimen_id=7916;

--Other scripts that could be used for issues:

--delete from ESE_CATCHES where mission='NED2014018' and setno =200 and spec=203;

--delete ese_lv1_observations where mission='CAR2021240' and setno=60 and spec=11 and specimen_id=13666;
--delete ese_specimens where mission='CAR2021240' and setno=60 and spec=11 and specimen_id=13666;

--DONE


select distinct(spec), min(to_number(data_value)) min_weight ,max(to_number(data_value)) max_weight
from ese_lv1_observations 
where lv1_observation = 'Weight' and mission = 'CAR2021240'
group by spec
order by spec;

--halibut weight in kg but should be in grams
--update ESE_LV1_OBSERVATIONS
--set data_value=7480 where mission='CAR2021240' and setno=24 and spec=30 and LV1_OBSERVATION='Weight' and data_value=7.48
--and specimen_id=9795;

--monkfish weight in kg but should be in grams
--update ESE_LV1_OBSERVATIONS
--set data_value=4360 where mission='CAR2021240'
--and setno=24 and spec=400 and LV1_OBSERVATION='Weight' and data_value=4.36
--and specimen_id=9513;



--OTHER POSSIBLE FIXES:
--ISSUE: MIN LOBSTER WEIGHT 0.1 for a 56mm lobster likely supposed to be 100g
--update ESE_LV1_OBSERVATIONS
--SET DATA_VALUE=100 WHERE LV1_OBSERVATION='Weight' AND SPECIMEN_ID=11124
--AND MISSION='NED2017020' AND SPEC=2550 AND DATA_VALUE=0.1;

--ISSUE: MIN LOBSTER WEIGHT 1.62 for a 133mm lobster likely supposed to be 1620g
--update ESE_LV1_OBSERVATIONS
--SET DATA_VALUE=1620 WHERE LV1_OBSERVATION='Weight' AND SPECIMEN_ID=25577
--AND MISSION='NED2017020' AND SPEC=2550 AND DATA_VALUE=1.62;

-- Locate the individual fish using the second selection, 
-- specifying the species and length.  Within the ESE find the
-- fish and determine if there is an obvious answer to the 
-- problem. Delete or update the length within the ESE routine so
-- that a record of your actions remain in the error repair log.

--select setno, spec, length from ESE_LV1_OBSERVATIONS where spec=&spec and length=&length;

--Weight for a skate was entered in KG, changed to Grams.  JE
--update ESE_LV1_OBSERVATIONS
--set data_value=226 where lv1_observation='Weight' and specimen_id=6623 
--and mission='TEL2017002' and spec=204 and data_value=.226;

--Other scripts for changes:
--insert into ese_catches values ('NED2015002', 35, 1191, 1,'Observations Completed.',null,null);

--insert into ESE_BASKETS values ('NED2015002', 35, 1191, 1, 0.015, 'Y');

--DONE
-------------------------------------------------------------------------------
-- 7) Check 6 list only the minimum values when there is an actual
-- value.  Sometimes the GSE leaves behind ghost records with 
-- null values for length.  Locate these in SQL and delete them rem in the GSE routine.
-- 


select setno, spec, size_class, specimen_id 
from ese_lv1_observations where mission = 'CAR2021240' 
and lv1_observation = 'Length' and (data_value is null or data_value = '0') 
order by spec;

--none

--DONE

-- specimens where no details collected or all detail removed.
select setno, spec,specimen_id from ese_specimens where mission = 'CAR2021240'
  MINUS 
select setno, spec,specimen_id from ese_lv1_observations where mission = 'CAR2021240';

--none

--Other possible repairs
--update ese_specimens 
--set spec=1191 where mission = 'NED2015002' and setno=35 and spec = 204 and specimen_id = 7450;
--update ese_specimens 
--set spec=1191 where mission = 'NED2015002' and setno=35 and spec = 204 and specimen_id = 7468;
--delete ese_specimens where mission = 'TEL2016002' and setno=17 and spec = 142 and specimen_id = 5565;

--DONE

-------------------------------------------------------------------------------
--8)  Even though you have already looked at minimum allowable 
-- values for each species, some acceptable lengths may still 
-- linger.  It is possible that a typo might make a 43 cm fish 
-- come out as 4 or 3 cm.  If you compare small lengths versus  
-- other observations such as weight and maturity, the result may 
-- indicate where a decimal was dropped.
--

select L.mission,L.setno,L.spec,L.size_class,L.specimen_id,L.length,W.weight, S.sex, M.maturity, F.fish_number, A.age_material_type
from
  (select mission, setno, spec, size_class, specimen_id, data_value length from ese_lv1_observations
   where lv1_observation = 'Length' and mission = 'CAR2021240' and to_number(data_value) < 10) L,
 (select mission, setno, spec, size_class, specimen_id, data_value weight from ese_lv1_observations
   where lv1_observation = 'Weight' and mission = 'CAR2021240') W,   
  (select mission, setno, spec, size_class, specimen_id, data_value sex from ese_lv1_observations
   where lv1_observation = 'Sex' and mission = 'CAR2021240') S,   
  (select mission, setno, spec, size_class, specimen_id, data_value maturity from ese_lv1_observations
   where lv1_observation = 'Maturity' and mission = 'CAR2021240') M, 
  (select mission, setno, spec, size_class, specimen_id, data_value fish_number from ese_lv1_observations
   where lv1_observation = 'Fish Number' and mission = 'CAR2021240') F, 
  (select mission, setno, spec, size_class, specimen_id, data_value age_material_type from ese_lv1_observations
   where lv1_observation = 'Age Material Type' and mission = 'CAR2021240') A      
where 
L.spec = W.spec and
L.spec = S.spec (+) and
L.spec = M.spec (+) and
L.spec = F.spec (+) and
L.spec = A.spec (+) and
L.specimen_id = W.specimen_id and
L.specimen_id = S.specimen_id (+) and
L.specimen_id = M.specimen_id (+) and
L.specimen_id = F.specimen_id (+) and
L.specimen_id = A.specimen_id (+)
order by L.spec, L.length
;


--no issues

--DONE

--if issue, example of fix below:

--update ese_lv1_observations
--set data_value=36 where mission='NED2014018' and lv1_observation='Length' and
--data_value=6 and specimen_id=22785 and setno=57 and spec=1191;

--
-------------------------------------------------------------------------------
-- 9) This script checks the detail table for distinct age_mat codes
-- You will require a species code list of otolith collected species, 
-- stomach and other special sampling for species requiring a fish number
-- Age_mat code 1 is used for otolith collected species (usually 
-- cod, haddock, white hake, silver hake, cusk, pollock and halibut) Yellowtail scales 
-- coded as '2' are collected during Georges Bank survey)
-- Age_mat code 9 is used to generate a fish number for stomachs or other special sampling
--
select distinct(data_value) from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Age Material Type';
--Should only be age mat 1 for all, we now take otolith for YT on Georges Bank

--no issues

--update ese_lv1_observations
--set data_value=1 where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=9 and specimen_id=65 and setno=2 and spec=60;

--update ese_lv1_observations
--set data_desc='Otolith Taken' where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=1 and specimen_id=65 and setno=2 and spec=60;

--update ese_lv1_observations
--set data_value=1 where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=2 and specimen_id=45271 and setno=158 and spec=10;

--update ese_lv1_observations
--set data_desc='Otolith Taken' where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=1 and specimen_id=45271 and setno=158 and spec=10;

--update ese_lv1_observations
--set data_value=1 where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=3 and specimen_id=47618 and setno=170 and spec=30;

--update ese_lv1_observations
--set data_desc='Otolith Taken' where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=1 and specimen_id=47618 and setno=170 and spec=30;



--IF ISSUE:

--update ese_lv1_observations
--set data_desc='Otolith Taken' where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value <> 1;

--update ese_lv1_observations
--set data_value=1 where mission='CAR2021240' and lv1_observation='Age Material Type' and data_value <> 1;


--DONE


select spec, data_value Age_Mat, count(*) from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Age Material Type' 
group by spec, data_value
order by spec; 


--discovered what happened to 4 cod captured on set 5 but no info entered; they were entered as Redfish size class 2
--changed spec to 10 and created new fish numbers, but discovered that individual weight issues also exist with 1 of the 4 cod
--changed length to match weight of one cod, and removed otolith manually


--update ESE_CATCHES
--set spec=10 where mission='CAR2021240' and setno=5 and spec=23 and size_class=2;
--update ESE_BASKETS
--set spec=10 where mission='CAR2021240' and setno=5 and spec=23 and size_class=2
--and basket_weight=5.76;
--update ESE_LV1_OBSERVATIONS
--set spec=10 where mission='CAR2021240' and setno=5 and spec=23 and size_class=2 and specimen_id in (1274,1282,1286,1289);
--update ESE_SPECIMENS
--set spec=10 where mission='CAR2021240' and setno=5 and spec=23 and size_class=2 and specimen_id in (1274,1282,1286,1289);
--update ESE_SPECIMENS
--set size_class=1 where mission='CAR2021240' and setno=5 and spec=10 and size_class=2 and specimen_id in (1274,1282,1286,1289);
--update ESE_LV1_OBSERVATIONS
--set size_class=1 where mission='CAR2021240' and setno=5 and spec=10 and size_class=2 and specimen_id in (1274,1282,1286,1289);
--update ESE_CATCHES
--set size_class=1 where mission='CAR2021240' and setno=5 and spec=10 and size_class=2;
--update ESE_BASKETS
--set size_class=1 where mission='CAR2021240' and setno=5 and spec=10 and size_class=2;
--update ESE_LV1_OBSERVATIONS
--set data_value=197 where mission='CAR2021240' and setno=5 and spec=10 and size_class=1 and lv1_observation='Fish Number' 
--and data_value=6 and specimen_id =1274;
--update ESE_LV1_OBSERVATIONS
--set data_value=199 where mission='CAR2021240' and setno=5 and spec=10 and size_class=1 and lv1_observation='Fish Number' 
--and data_value=8 and specimen_id =1286;
--update ESE_LV1_OBSERVATIONS
--set data_value=200 where mission='CAR2021240' and setno=5 and spec=10 and size_class=1 and lv1_observation='Fish Number' 
--and data_value=9 and specimen_id =1289;

--other possible fixes:
--update ese_lv1_observations
--set data_desc='Otolith taken' where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=2 and data_desc='Scales taken' and spec=11 and specimen_id = 23481;

--update ese_lv1_observations
--set data_value=1 where mission='CAR2021240' and lv1_observation='Age Material Type' and
--data_value=2 and data_desc='Otolith taken' and spec=11 and specimen_id = 23481;


--DONE

select distinct setno, spec, data_value
from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Age Material Type' and
to_number(data_value)=1 and spec not in(10,11,12,14,15,16,30,60)
order by spec;

--DONE

-- correct any erroneous age_mat codes using the following query:
-- update gs_entry_detail
-- set age_mat=x where spec=xxxx and age_mat=x;
-- An age_mat other than "1" can `be presumed to be in error unless a remarks exists in detail saying that otolith wasn't collected

  
select F.mission,F.setno,F.spec,F.size_class,F.specimen_id, F.fish_number, A.age_material_type
from
  (select mission, setno, spec, size_class, specimen_id, data_value fish_number from ese_lv1_observations
   where lv1_observation = 'Fish Number' and mission = 'CAR2021240') F, 
  (select mission, setno, spec, size_class, specimen_id, data_value age_material_type from ese_lv1_observations
   where lv1_observation = 'Age Material Type' and mission = 'CAR2021240') A      
where 
F.spec = A.spec and
F.specimen_id = A.specimen_id and
F.fish_number IS NOT NULL and 
A.age_material_type IS NULL
order by F.spec
;
--DONE

select F.mission,F.setno,F.spec,F.size_class,F.specimen_id, F.fish_number, A.age_material_type
from
  (select mission, setno, spec, size_class, specimen_id, data_value fish_number from ese_lv1_observations
   where lv1_observation = 'Fish Number' and mission = 'CAR2021240') F, 
  (select mission, setno, spec, size_class, specimen_id, data_value age_material_type from ese_lv1_observations
   where lv1_observation = 'Age Material Type' and mission = 'CAR2021240') A      
where 
F.spec = A.spec and
F.specimen_id = A.specimen_id and
F.fish_number IS NULL and
A.age_material_type IS NOT NULL
order by F.spec
;
--none
--DONE



-------------------------------------------------------------------------------
-- 10) rem Detail Table:  Check 10a.
---
-- Check the det_comments field of the detail table to correct any typos 
-- 
select mission, setno, spec, size_class, specimen_id, data_value comments from ese_lv1_observations
  where lv1_observation = 'Comments' and mission = 'CAR2021240'
order by data_value;

--removed the . and , comments used for herring
--DELETE ESE_LV1_observations WHERE Mission='CAR2021240' AND SPEC=60 AND lv1_observation='Comments' and DATA_VALUE IN (',','.'.'1','2');

--DONE



--Other scripts for fixes:

--update ESE_CATCHES 
--set spec=158 where mission='TEL2016002' and spec=150;

--update ESE_BASKETS 
--set spec=158 where mission='TEL2016002' and spec=150;

--update ESE_LV1_OBSERVATIONS
--set spec=158 where mission='TEL2016002' and spec=150;

--update ESE_SPECIMENS
--set spec=158 where mission='TEL2016002' and spec=150;

--delete from ESE_BASKETS where mission='TEL2016002' and spec=158 and setno=88 and basket_weight=0.002;

--insert into ese_catches values ('TEL2016002', 88, 150, 1,'Observations Completed.',null,null);

--insert into ESE_BASKETS values ('TEL2016002', 88, 150, 1, 0.002, 'Y');

--update ESE_LV1_OBSERVATIONS
--set spec=150 where mission='TEL2016002' and spec=158 and specimen_id=30368;

--UPDATE ESE_SPECIMENS
--SET SPEC=150 WHERE mission='TEL2016002' and spec=158 and specimen_id=30368;


--DONE


-------------------------------------------------------------------------------
-- 11) rem Detail Table:  Check 11.
--
-- Check appropriate sex entries by species.
--

select distinct(data_value) from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Sex'
order by data_value;

--DONE

select spec, data_value sex, count(data_value) howmany from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Sex'
group by spec, data_value
order by spec, data_value;

--no issues

--If required:
--update ESE_LV1_observations
--set DATA_VALUE='0' where Mission='CAR2021240' and setno=48 and spec=11 
--and specimen_id = 12377 and lv1_observation = 'Sex';

--update ESE_LV1_observations
--set DATA_DESC='Undetermined' where Mission='CAR2021240' and setno=48 and spec=11 
--and specimen_id = 12377 and lv1_observation = 'Sex';

--update ESE_LV1_observations
--set DATA_VALUE='0' where Mission='CAR2021240' and setno=42 and spec=11 
--and specimen_id = 10730 and lv1_observation = 'Sex';

--update ESE_LV1_observations
--set DATA_DESC='Undetermined' where Mission='CAR2021240' and setno=42 and spec=11 
--and specimen_id = 10730 and lv1_observation = 'Sex';

--update ESE_LV1_observations
--set DATA_VALUE='2' where Mission='CAR2021240' and setno=1 and spec=23 
--and specimen_id = 6 and lv1_observation = 'Sex';

--update ESE_LV1_observations
--set DATA_DESC='Female' where Mission='CAR2021240' and setno=1 and spec=23 
--and specimen_id = 6 and lv1_observation = 'Sex';

--DONE

-- check maturity codes

select distinct(data_value) from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Maturity'
order by data_value;

--DONE

select spec, data_value maturity, count(data_value) howmany from ese_lv1_observations 
where mission = 'CAR2021240' and lv1_observation = 'Maturity'
group by spec, data_value
order by spec, data_value;

--A few maturities were determined for John Dory as they were ripe.

--DONE
-------------------------------------------------------------------------------
-- 12) Rem Catch Table:  Check12.

-- Check for duplicate records in ese_catches.
select setno, spec, size_class from 
(select setno, spec, size_class, count(*) howmany from ese_catches where mission = 'CAR2021240' 
 group by setno, spec, size_class)
where howmany > 1;
-- no issues

--Some species are sampled in 2 size classes when they exist;
-- check with select * from ese_catches where spec=xx and setno=xx


--DONE
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
--13) This query checks for basket weights that differ from the detailed sampled weight by +25% or -25%
--    using unspecified a and b values

drop table temp_weight_check1;
drop table temp_weight_check2;
drop table temp_weight_check3;
drop table temp_weight_check4;

create table temp_weight_check1 as
  (select spec, setno, size_class, sum(basket_weight)sampled_weight 
  from ese_baskets 
  where mission = 'CAR2021240' and sampled='Y'
  group by spec, setno, size_class);

create table temp_weight_check2 as 
(select spec, setno, size_class, (sum(to_number(data_value))/1000) detail_weight_collected from ese_lv1_observations
where mission = 'CAR2021240' and lv1_observation = 'Weight'
group by spec, setno, size_class
);


create table temp_weight_check3 as
select
    a.spec, a.setno, a.size_class,
    sum((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified))/1000) detail_weight_calculated
  from 
    ese_detail a, ese_sampreq_species b
 where 
    a.mission = b.mission and
    a.spec= b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is null and
    b.length_weight_a_unspecified is not null and
    b.length_weight_b_unspecified is not null
group by a.spec, a.setno, a.size_class
;




create table temp_weight_check4 as
  (select 
    a.spec, a.setno, a.size_class, (a.detail_weight_collected+b.detail_weight_calculated) detail_weight
  from 
    temp_weight_check2 a, temp_weight_check3 b
  where 
    a.spec=b.spec and 
    a.setno=b.setno and 
    a.size_class = b.size_class and
    detail_weight_calculated is not null);

select 
    a.spec, a.setno, a.size_class, a.sampled_weight, round(b.detail_weight,3) detail_weight, 
    abs(round((a.sampled_weight-b.detail_weight)/a.sampled_weight,2))*100 percent_difference 
  from 
    temp_weight_check1 a, temp_weight_check4 b
  where 
    a.spec= b.spec and 
    a.setno= b.setno and
    a.size_class= b.size_class
order by abs(round((a.sampled_weight-b.detail_weight)/a.sampled_weight,2))*100 desc;

--see excel file S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx

--DONE

-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
--15)Tow distance and speed check

Drop table temp_distinf;

create table temp_distinf as (select mission,setno,start_date,start_time,end_date,end_time,slat,slong,elat,elong, experiment_type_code from ese_sets where mission = 'CAR2021240');

alter table temp_distinf add (calc_dist number,calc_speed number,calc_time number);

update temp_distinf set calc_dist = 
  sqrt(( 
         POWER(ABS(((trunc(slat/100)) * 60 + (slat -(trunc(slat/100)*100))) - ((trunc(elat/100)) * 60 + (elat -(trunc(elat/100)*100)))),2)
         + 
         POWER((( ABS(((trunc(slong/100)) * 60 + (slong -(trunc(slong/100)*100)))-((trunc(elong/100)) * 60 + (elong -(trunc(elong/100)*100)))) )
           * Cos(((trunc(slat/100)) * 60 + (slat -(trunc(slat/100)*100))) / 60 * 3.1416 / 180)),2) 
      ));
   
update temp_distinf set Calc_time=24*(to_number(to_date(end_date||end_time,'DDMMYYYYHH24MI')-to_date(start_date||start_time,'DDMMYYYYHH24MI')));

update temp_distinf set calc_speed=calc_dist/calc_time where calc_time!=0;

select mission,setno,experiment_type_code, start_date,start_time stime,end_date,end_time,
slat,elat,slong,elong,round(calc_dist,2) calc_dist,round(calc_speed,2) calc_speed,round(calc_time,2) calc_time 
from temp_distinf where (calc_speed not between 3.2 and 3.9) or (calc_dist not between 1.5 and 1.9) or (calc_time not between .2 and .5)
order by mission, setno;

--see excel file S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx

--DONE




--Other fixes:
--update ESE_SETS
--set END_DATE=24022016 where Mission='TEL2016002' and setno=5 and start_date=24022016 and end_date=2402016;

--update ESE_SETS
--set speed=3.18 where Mission='TEL2016002' and setno=29 and speed=3.55;

--update ESE_SETS
--set end_time= 1607 where Mission='TEL2016002' and setno=49 and end_time=1617;

--update ESE_SETS
--set dist=1.22 where Mission='TEL2016002' and setno=53 and dist=1.17;

--Error in Elong sometimes entered and not fixed (SHOULD BE CAPTURED AT SEA!)
--update ESE_Sets
--set ELONG=6248.63 where Mission='NED2014018' and setno=87;
--update ESE_Sets
--set SPEED=3.52 where Mission='NED2014018' and setno=87;
--update ESE_Sets
--set DIST=1.76 where Mission='NED2014018' and setno=87;

--Incorrect Experiment_Type_Code setno 26
--update ESE_SETS
--set EXPERIMENT_TYPE_CODE =3 where Mission='NED2014018' and SETNO=26;

--Incorrect End_Time
--update ESE_SETS
--set END_TIME=1437 where Mission='NED2014018' and SETNO=33 and END_TIME=1433;


-------------------------------------------------------------------------------
-- 16) this script selects duplicate fish numbers from the entry detail table

select spec, fish_number, duplicates from 
(select spec, data_value fish_number, count(*) duplicates from ese_lv1_observations
   where lv1_observation = 'Fish Number' and mission = 'CAR2021240'   
group by spec, data_value)
where duplicates > 1;



--Herring have duplicate fish numbers due to the mission file change after set #2.  Will assign unique fish numbers to all fish
--saved from set #2

--update ESE_LV1_OBSERVATIONS
--set data_value=(data_value+287) where mission='CAR2021240' and setno=2 and spec=60 and size_class=1 and lv1_observation='Fish Number'
--and specimen_id in ('765','767','768','770','772','773','774','775','776','777','779','781','782','788','791','793','798','807',
--'811','830','838');

--DONE

--This script selects specimen_id duplicates (if necessary):

select spec, specimen_id, duplicates from 
(select spec, specimen_id, count(*) duplicates from ese_lv1_observations
   where mission = 'CAR2021240' and lv1_observation='Length'  
group by spec, specimen_id)
where duplicates > 1;


-------------------------------------------------------------------------------
-- 17) Test for duplicate records in ese_baskets table
select setno, spec,size_class, basket_weight, duplicates from
(select setno, spec,size_class , basket_weight, count(*) duplicates 
from ese_baskets where mission = 'CAR2021240' 
group by setno, spec,size_class, basket_weight)
where duplicates > 1
order by setno;

--none


--DONE

--update ESE_BASKETS
--set BASKET_WEIGHT=0.02  where Mission='NED2014018' and SETNO=47 and SPEC=2526 and
--BASKET_WEIGHT=0.01 and SIZE_CLASS=1;


-------------------------------------------------------------------------------
-- 18) test for duplicate records in ese_catches table
select setno, spec,size_class from
(select setno, spec,size_class, count(*) duplicates
from ese_catches where mission = 'CAR2021240' 
group by setno, spec,size_class)
where duplicates > 1;

--none
--DONE
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- 20) -- Check individual length and weights   
select
    a.spec, a.setno, a.length, a.weight, a.sex, a.specimen_id, 
    round((b.length_weight_a_male*power(a.length,b.length_weight_b_male)),2) calc_weight,
    abs(round(((b.length_weight_a_male*power(a.length,b.length_weight_b_male) - weight))/weight,2)) difference
  from 
    ese_detail a, ese_sampreq_species b
  where 
    a.mission = b.mission and
    a.spec=b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is not null and
    a.sex = 1 and  -- male
    a.spec in (10,11,12,14,15,16,23,60,220)
order by abs(round(((b.length_weight_a_male*power(a.length,b.length_weight_b_male) - weight))/weight,2)) desc;

--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx
 

select
    a.spec, a.setno, a.length, a.weight, a.sex, a.specimen_id,
    round((b.length_weight_a_female*power(a.length,b.length_weight_b_female)),2) calc_weight,
    abs(round(((b.length_weight_a_female*power(a.length,b.length_weight_b_female) - weight))/weight,2)) difference
  from 
    ese_detail a, ese_sampreq_species b
  where 
    a.mission = b.mission and
    a.spec=b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is not null and
    a.sex = 2 and  -- female
    a.spec in (10,11,12,14,15,16,23,60,220)
order by abs(round(((b.length_weight_a_female*power(a.length,b.length_weight_b_female) - weight))/weight,2)) desc;


--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx

select
    a.spec, a.setno, a.length, a.weight, a.sex, a.specimen_id,
    round((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified)),2) calc_weight,
    abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) difference
  from 
    ese_detail a, ese_sampreq_species b
  where 
    a.mission = b.mission and
    a.spec=b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is not null and
    a.sex =0 and --unspecified
    a.spec in (10,11,12,14,15,16,23,60,220)
order by abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) desc;

--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx

select
    a.spec, a.setno, a.length, a.weight, a.sex, a.specimen_id,
    round((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified)),2) calc_weight,
    abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) difference
  from 
    ese_detail a, ese_sampreq_species b
  where 
    a.mission = b.mission and
    a.spec=b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is not null and
    a.sex is null and --unsexed
    a.spec in (10,11,12,14,15,16,23,60,220)
order by abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) desc;

--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx

--Not Necessary?
select
    a.spec, a.setno, a.length, a.weight, a.sex, a.specimen_id,
    round((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified)),2) calc_weight,
    abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) difference
  from 
    ese_detail a, ese_sampreq_species b
  where 
    a.mission = b.mission and
    a.spec=b.species_code and
    a.mission = 'CAR2021240' and
    a.weight is not null and
    --a.sex = 0 --and  -- Other species
     a.spec not in (10,11,12,14,15,16,23,60,220)
order by abs(round(((b.length_weight_a_unspecified*power(a.length,b.length_weight_b_unspecified) - weight))/weight,2)) desc;


--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\CAR2021240_Basket_Tow_Len_wt_Check.xlsx
 --DONE
-------------------------------------------------------------------------------
-- 22) Check for null catch in entry tables before loading
select distinct mission, setno, spec, size_class from ese_lv1_observations
where (mission, setno, spec, size_class) in
(select distinct mission, setno, spec, size_class from ese_lv1_observations where mission = 'CAR2021240'
minus
select distinct mission, setno, spec, size_class from ese_catches where mission = 'CAR2021240')
order by mission, setno, spec, size_class;

--NONE


--Other scripts
--INSERT INTO ESE_BASKETS VALUES ('VEN2018002',32,42,1,1.8,'Y');
--insert into ese_catches values ('NED2015002',40,143,1,NULL,NULL,NULL); 
--delete ESE_LV1_OBSERVATIONS where mission='TEL2016002' and setno=88 and spec=930; 

--DONE
-------------------------------------------------------------------------------
-- 23) This is a check for count on strata (should be at least 2 per stratum except
-- 443=4; 444=8; 445=4)
-- If count is less than 2 per stratum run the query for the 1st leg also
-- Together there should be at least 2 sets per stratum
--
select strat, count(*) howmany from ese_sets where mission = 'CAR2021240' and experiment_type_code=1
group by strat
order by strat;

--5Z9 ONLY HAS 1 SET.  THIS WAS DUE TO TEARUP JUST BEFORE LEAVING STRATA, AND ASSURANCE THAT SETS FROM CAR2021241 COULD
--BE USED TO ADD TO NUMBERS OF SETS PER STRATA


--Other scripts if needed:
--update ESE_SETS
--set strat='453' where mission='NED2017020' and setno=249 and strat='466';

--DONE
-------------------------------------------------------------------------------
-- 24) this script selects setno,spec and unweighed_baskets
-- from the catches table to find species that were tallied as unweighed (ex. dogfish)
select setno,spec,unweighed_baskets
from ese_catches where mission = 'CAR2021240'
and unweighed_baskets is not null;



--OPTIONS
--update ESE_CATCHES 
--set NUMBER_CAUGHT=8 where mission='CAR2021240' and setno=79 and spec=6111 and UNWEIGHED_BASKETS=8;

--update ESE_CATCHES
--set UNWEIGHED_BASKETS=null where mission='CAR2021240' and setno=79 and spec=6111 and UNWEIGHED_BASKETS=8;




--DONE

-------------------------------------------------------------------------------
-- 25) this script checks for any weights in the detail table entered as '0'or <0
select mission, setno, spec, size_class, specimen_id, data_value weight from ese_lv1_observations
   where lv1_observation = 'Weight' and mission = 'CAR2021240' and to_number(data_value) <= 0;
--none
--DONE

---------------------------------------------------------------------------------
--26) --ESE_SETS table duration check
select mission, setno, start_date, start_time, end_date, end_time,  dist, speed, experiment_type_code, note from ese_sets
where mission='CAR2021240' and experiment_type_code=1;

--REMINDER:
   
--BE SURE TO LOAD AZMP TEMPERATURE AND SALINITY DATA BEFORE LOADING TO GS TABLES 
--S:\Science\Shared\RV Surveys\Shared\Groundfish Surveys\Groundfish Database Edits\Load Excel table into Access and then Oracle Instructions.docx
--Import hydro data to Access from Excel, then export from Access to ODBC Database (Machine Data Source - PTRAN)
--UPDATE THE DEPTH FROM METERS TO FATHOMS IN THE T_S TABLE BEFORE LOADING

--SEE FILE UPDATE_TEMP_SAL_FROM_TABLE.SQL   


--NO OCEANOGRAPHIC SETS COMPLETED ON CARTIER SUMMER SURVEY


--check duration on type 1 tows -THIS WOULD HAVE TO BE RUN AFTER LOAD TO GS TABLES
select mission, setno,time, sdate, etime, dur, type from gsinf
where mission='CAR2021240' and dur>20 and type=1;

--3 sets listed as 21 m in duration. Likely a result of the mate marking down time after set had ended
--Updated manually in GSINF to 30 min tows


select mission, setno,time, sdate, etime, dur, type from gsinf
where mission='CAR2021240' and dur <16 and type=1;

--none

--DONE





--AFTER DATA IS EDITED AND LOADED TO PRODUCTION THROUGH GROUNDFISH FORMS APPLICATIONS, BE SURE TO LOAD THE INFO INTO GSCRUISELIST AND GSMISSIONS

--SEE SQL FILE GSCRUISELIST_GSMISSIONS APPEND_SURVEY.SQL  
--OK

--ALSO UPDATE ANY SPECIES ID'S THAT WERE COMPLETED ONSHORE FOR UNIDENTIFIED SPECIES

--SCRIPTS IF NEEDED:


--delete gscat where mission='TEL2020002' and setno in (3,9) and spec 8323;
--delete ese_catches where mission='TEL2020002' and setno in (3,9) and spec 8323;
--delete ese_basekets where mission='TEL2020002' and setno in (3,9) and spec 8323;
--OK

--update lv1_observations
--set lv1_observation='Weight' where mission='CAR2021240' and setno=242 and specimen_id=65668 and lv1_observation='Comments';

--update lv1_observations
--set data_value=114 where mission='CAR2021240' and setno=242 and specimen_id=65668 and lv1_observation='Weight';

--update lv1_observations
--set lv1_observation='Weight' where mission='CAR2021240' and setno=242 and specimen_id=65678 and lv1_observation='Comments';

--update ese_catches
--set NOTE='ID at sea but not kept; should be Pholis sp. but no code currently exists'
--where MISSION='VEN2018002' AND SETNO=48 and spec=633;

--update gsdet 
--set REMARKS='ID at sea but not kept; should be Pholis sp. but no code currently exists' where MISSION='VEN2018002' AND SETNO=48 and spec=633 AND flen=5;

--update gscat
--set REMARKS='ID at sea but not kept; should be Pholis sp. but no code currently exists' where MISSION='VEN2018002' AND SETNO=48 and spec=633 and totwgt=0.001;

--SOME SPECIES WERE UNIDENTIFIED; SOME FIXES BELOW.

--UPDATE ESE_CATCHES
--SET SPEC=6900 WHERE mission='NED2019002' and spec = 9991 and setno = 222 AND SIZE_CLASS=1;

--UPDATE ESE_BASKETS
--SET SPEC=6900 WHERE mission='NED2019002' and spec = 9991 and setno = 222 AND SIZE_CLASS=1;

--update gscat
--SET SPEC=6900 WHERE mission='NED2019002' and spec = 9991 and setno = 222 AND SIZE_CLASS=1;


      
   