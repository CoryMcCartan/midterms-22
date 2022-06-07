
#Codebook for U.S. House Returns 1976–2020

The data file `1976-2020-house` contains constituency (district) returns for elections to the U.S. House of Representatives from 1976 to 2020.  The data source is the document "[Statistics of the Congressional Election](https://history.house.gov/Institution/Election-Statistics/)," published biennially by the Clerk of the U.S. House of Representatives. 2018 data comes from official state election websites, and for Kansas, come from Stephen Pettigrew and the Kansas Secretary of State office (in some cases, they are marked as unofficial, and will be updated at a later time).

All string variables are in upper case. 

##Variables 
The variables are listed as they appear in the data file.  

###year
 - **Description**: year in which election was held
 
---------------

###state
 - **Description**: state name

 ---------------
 
###state_po
 - **Description**: U.S. postal code state abbreviation

 ---------------
 
###state_fips
 - **Description**: State FIPS code

----------------

###state_cen
 - **Description**: U.S. Census state code

 ---------------
 
### state_ic
 - **Description**: ICPSR state code

 --------------- 
 
###office
- **Description**: U.S. House (constant)
  
---------------

### district
 - **Description**: district number
 - ****Note****:	At-large districts are coded as 0 (zero).

---------------

### stage
 - **Description**: electoral stage
 - **Coding**: 

| code | definition |
|:---|:---|
| "gen" | general elections |
| "pri" | primary elections |

 - **Note**: Only appears in special cases. Consult original House Clerk report for these cases.

----------------

### special
- **Description**: special election
- Coding  

| code | definition |
|:---|:---|
| "TRUE" | special elections |
| "FALSE" | regular elections |

----------------

### candidate
  - **Description**: name of the candidate
  - **Note**: The name is as it appears in the House Clerk report.

----------------

### party
- **Description**: party of the candidate (always entirely lowercase)
  - **Note**: Parties are as they appear in the House Clerk report.  In states that allow candidates to appear on multiple party lines, separate vote totals are indicated for each party.  Therefore, for analysis that involves candidate totals, it will be necessary to aggregate across all party lines within a district.  For analysis that focuses on two-party vote totals, it will be necessary to account for major party candidates who receive votes under multiple party labels. Minnesota party labels are given as they appear on the Minnesota ballots. Future versions of this file will include codes for candidates who are endorsed by major parties, regardless of the party label under which they receive votes.
	
----------------
	
### writein
- **Description**: vote totals associated with write-in candidates
- **Coding**:

| code | definition |
|:---|:---|
| "TRUE" | write-in candidates |
| "FALSE" | non-write-in candidates |

-----------------

### mode
- **Description**: mode of voting; states with data that doesn't break down returns by mode are marked as "total"

----------------
	
### candidatevotes 
 - **Description**: votes received by this candidate for this particular party

----------------

### totalvotes
 - **Description**: total number of votes cast for this election

 ----------------


### fusion_ticket
 - **Description**: A TRUE/FALSE indicator as to whether the given candidate is running on a fusion party ticket, which will in turn mean that a candidate will appear multiple times, but by different parties, for a given election. States with fusion tickets include Connecticut, New Jersey, New York, and South Carolina. 

 ----------------
### unofficial
- **Description**: TRUE/FALSE indicator for unofficial result (to be updated later); this appears only for 2018 data in some cases

----------------

### version  
- **Description**: date when this dataset was finalized

##NOTES: 

candidatevotes: for uncontested races, value is set to 1 in FL. Should user want to set a higher value for analysis 
purposes, consider setting the value as the maximum for a given state-year. The code in R would be the following:
df <- read.csv("1976-2018-house.csv", stringsAsFactors = FALSE)
df <- df %>%
   group_by(state_po,district) %>%
   mutate(max_st_year_vote = max(candidatevotes, na.rm=T)

The following code should be used if the user would like to assume that uncontested candidates would have recieved
as many votes as the best contested candidate.

district: district is set to 0 for single member states. 

party and candidate: candidate - party combinations are recorded as they were on the state elections website. This
means that for states where the same candidate might appear on multiple parties, like in NY, they are recorded as
such. Therefore, for users interested in finding the primary party, run the following code: 

df <- read.csv("1976-2020-house.csv", stringsAsFactors = FALSE)
df$district <- str_pad(df$district, width=2, pad="0", side="left)
df$state_fips <- str_pad(df$state_fips, width=2, pad="0", side="left)
df$GEOID <- paste(df$state_fips, df$district, sep="")
df_max <- df %>%
   group_by(candidate, GEOID, year) %>%
   slice(which.max(candidatevotes)
df_sum <- df %>%
   group_by(candidate, GEOID, year) %>%
   aggregate(candvotes_sum = sum(candvotes))


 
