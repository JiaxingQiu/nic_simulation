Model Data Columns

VitalID		Vital sign file ID
VitalTime	Vital sign file timestamp
EGA		Estimated gestational age in weeks
Day		Day of life (0 = birthday)
DayTime		Time of day in days
Age		Age in days from birthdate/time
PMA		Post-menstrual age in weeks
Event		Days to death less than 7 days
DaysToDeath	Days to death for events
hr.mean		HR mean
hr.std		HR standard deviation
hr.max		HR maximum
hr.min		HR minimum
sp.mean		SPO2 mean
sp.std		SPO2 standard deviation
sp.max		SPO2 maximum
sp.min		SPO2 minimum

Demographic Data Columns

VitalID		Vital sign file ID
EGA		Estimated gestational age in weeks
BWT		Birth weight in grams
Male		Sex		
Apgar1		Apgar 1-minute
Apgar5		Apgar 5-minute
Vaginal		Vaginal delivery
C-section	C-section delivery
Steroids	Antenatal steroids
InBorn		Born in hospital
BirthHC		Head circumference 
Multiple	Multiple births
Black		Race		
Hispanic	Race		
White		Race
MaternalAge	Maternal age in years		

Outcome Data Columns

VitalID		Vital sign file ID
Post2012	Born 2012 or later (start of POWS redcap database)
Monitor		1=Old GE Monitor <2017 2=New GE Montitor
DiedNICU	Died within 7 days of NICU discharge
Died		Died in hospital
DeathAge	Age at death in days
DeathPMA	PMA at death in weeks
BirthTime	Time of birth in days
AdmitAge	Age of admission to NICU in days
DischargeAge	Age of dischage from NICU in days
LengthOfStay	Length of stay in NICU in days
Home		Dischargd to home from NICU

HR/SPO2 10-minute Data Columns

VitalID		Vital sign file ID
VitalTime	Vital sign file timestamp
1-300		Every 2-second sample 
