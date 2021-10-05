# School_Abstenteeism_Based_Influenza_Surveillance_Simulation_Study

Thesis title: ATQ: Alarm time quality, an evaluation metric for assessing timely epidemic detection models within a school absenteeism-based surveillance system

Thesis background: 
Wellington-Dufferin-Guelph Public Health (WDGPH) has conducted an absenteeism-based influenza surveillance program in the WDG region of Ontario, Canada since 2008, using a 10% absenteeism threshold to raise an alarm for the implementation of mitigating measures.  A recent study indicated that model-based alternatives provided improved alarms, however evaluation was primarily based on alarm accuracy, measured by the false alarm rate (FAR), and failed to optimize timeliness.  Here, a new metric that simultaneously evaluates epidemic alarm accuracy and timeliness is proposed.  The alarm time quality (ATQ) metric is investigated on both a simulated and real data set, however only the code for the simulated data is provided here for privacy reasons.

The ATQ assessed alarms on a gradient, where alarms raised incrementally before or after an optimal day were considered informative, but were still penalized for lack of timeliness.  Summary statistics of ATQ, average alarm time quality (AATQ), and first alarm time quality (FATQ) were used for model evaluation and selection criterion. Alarms raised by ATQ and FAR-selected models were compared.  Here, a simulation study representative of wDG population and influenza demographics was conducted for evaluation of the proposed metric.  Distributed lag logistic regression models were used as epidemic detection models.

The simulation study consisted of three main sequential parts: 1) a population of individuals was generated; 2) an influenza epidemic was simulated over the population for a given year; 3) a random probabilistic model was applied to the simulated population and epidemic to generate laboratory-confirmed influenza case and simulated school absenteeism data. 

Population Simulation (corresponding code is /R/01_simulate household data.R)
Demographics from the 2016 census for the area serviced by WDG such as distribution of number of household members, households with and without children, and age category were used to create a population representative of WDG.  Due to computing power required for the epidemic simulation component, the population size was scaled down, and was driven by the set number catchment areas, in this study we chose 16 catchment areas. The number of schools within each catchment area and student population sizes were drawn from Gamma distributions.

Epidemic Simulation (corresponding code is /R/02_simulate epidemic.R)
Annual influenza epidemics were simulated using a homogeneous spatial individual level model within a susceptible, infectious, and removed framework.   The start of an epidemic was generated from a normal distribution. Epidemics were initiated by randomly infecting two individuals from each of the 16 catchment areas (32 individuals in total), with a random infection time that is within 14 days of the epidemic start time.

Laboratory Case Confirmation and Absenteeism System (corresponding code is /R/03_simulate lab confirmed cases and absenteeism.R)
Absenteeism data was simulated using probabilistic models conditional on if a student was infected. Similarly, since not all individuals that are ill seek medical attention, laboratory confirmation data was simualted using a probabilistic model.

The remaining code creates logistic regression models for epidemic detection with various probability thresholds and lags.  Model tuning parameters are selected based on the optimization of ATQ, and FAR metrics.
