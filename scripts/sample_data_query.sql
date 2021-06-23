select 
de.dataelementid, 
de."name" de_name,  
pe.startdate,
dv.value,
ou.organisationunitid, 
ou."name" facility_name,
ou3.organisationunitid district_orgunitid,
ou3."name" district_ou_name
from dataelement de 
join datavalue dv on de.dataelementid = dv.dataelementid 
join "period" pe on dv.periodid = pe.periodid 
join periodtype pet on pe.periodtypeid = pet.periodtypeid 
-- PHUs are Level 4
join organisationunit ou on dv.sourceid = ou.organisationunitid
-- Join to organisationunit to get parent in Level 3 (Chiefdom)
left join organisationunit ou2 on ou.parentid = ou2.organisationunitid 
-- Join to organisationunit again to get parent in Level 2 (District)
left join organisationunit ou3 on ou2.parentid = ou3.organisationunitid 
-- Individual facility levels (PHU)
where ou.hierarchylevel = 4
-- Monthly data
and pet."name" = 'Monthly' 
-- Filtering to a single district for demonstration purposes
and ou3."name" = 'Kenema' 
-- Grabbing births and low birth weight counts for aggregation later
and de."name" in ('Live births', 'Low birth weight in PHU') 