### Fragile Families cohort preprocessing script
# Pierre O Jacquet 2026
# This pulls the relevant variables into a data frame and performs some simple checks
# Note that more variables are selected than are used in the study to make it easier
# to conduct any additional analyses for fellow researchers using our script

#### Call packages, and read data

library(dplyr)
library(tidyverse)
library(car)
library(haven)
library(PerformanceAnalytics)
library(labelled)

setwd("/Users/pierre.jacquet/Library/CloudStorage/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")
setwd("C:/Users/PJacquet/Dropbox/DGAEFS/RESEARCH/People/Heloise/Child Development/Short-term mindset/data")

mydf <- read_sav("FF_allwaves_2020v2_SPSS.sav")

#### Select variables for analysis

mydf.raw <- mydf %>%
  select(                                  # Wave 1
    
    idnum,                            # Family ID
    cm1bsex,                          # Sex of child
    cm1lbw,                           # Low birth wgt?
    m1g1,                             # Mother health
    m1f5,                             # Streets safe at night?
    cm1fedu,                          # Father education                        R
    cm1edu,                           # Mother education                        R  
    cm1hhinc,                         # Household income                        R
    cm1inpov,                         # Poverty threshold

    # Wave 2

    m2a3,                             # Child live with mother?
    m2a8e,                            # Rel. ended?
    m2b2,                             # Child health
    m2b15,                            # Separation nr
    m2b18a:m2b19a,                    # Activities and physical punishment
    m2b21,                            # Child cared for by someone else?
    m2b23,                            # How many arrangements
    m2b28b,                           # How many change in arrangements
    cm2ffevjail,                      # Father in jail by 1st yr
    m2h1,                             # Moved?
    m2h1a,                            # How many times moved
    m2j12:m2j15c,                     # Mother depression
    m2j15c1:m2j15f,
    m2j16:m2j20g,                     # Mother anxiety
    cm2gad_case,                      # Mother anxiety criteria
    cm2md_case_lib,                   # Mother depression criteria
    cm2edu,                           # Mother education
    cf2edu,                           # Father education
    cm2hhinc,                         # Household income
    cm2povco,                         # Poverty threshold

    # Wave 3

    m3a2,                             # Child live with mother?
    m3a6,                             # Living apart, but together now?
    m3a8,                             # Rel. ended?
    m3b2,                             # Child health
    m3b3,                             # Separation nr
    m3b4a:m3b5a,                      # Activities and physical punishment
    m3b7,                             # Child cared for by someone else?
    m3b7b,                            # How many arrangements
    m3b13,                            # How many change in arrangements
    cm3ffevjail,                      # Father in jail by 1st yr
    m3i1,                             # Moved?
    m3i1a,                            # How many times moved
    m3j5:m3j17,                       # Mother depression
    m3j18:m3j27g,                     # Mother anxiety
    m3j44a:m3j44f,                    # Mother impulsivity
    cm3gad_case,                      # Mother anxiety criteria
    cm3md_case_lib,                   # Mother depression criteria
    cf3edu,                           # Mother education
    cm3edu,                           # Father education
    cm3hhinc,                         # Household income
    cm3povco,                         # Poverty threshold
    cm3adult,                         # Household composition
    cm3kids,                          # Household composition
    p3b3,                             # Routines
    p3b5,
    p3b6a,
    p3b8,
    p3b11,
    p3b13,
    p3b16,
    p3c1a:p3c1h,                      # Nr of toys
    p3c7,                             # Diff. people care
    p3j1:p3j19,                       # Discipline
    p3l1:p3l7,                        # Exposure to violence
    p3m1:p3m50,                       # CBCL + ASBI
    o3t1:o3t16,                       # Observed parent-child interactions
    ch3cbmi,                          # Child BMI
    ch3att_codeabc,                   # Attachment category
    ch3att_b1:ch3att_b3,              # Attachment scores
    ch3emp_totjob,                    # Nr of jobs
    ch3cc_totarr,                     # Nr of arrangements

    # Wave 4

    m4a2,                             # Child live with mother?
    m4a6,                             # Living apart, but together now?
    m4a8,                             # Rel. ended?
    m4b2,                             # Child health
    m4b3,                             # Separation nr
    m4b4a1:m4b4a8,                    # Activities
    m4b5:m4b5a,                       # Physical punishment
    m4b8h,                            # School program for 8+ h
    m4b9,                             # Child cared for by someone else?
    m4b9b,                            # How many arrangements
    cm4ffevjail,                      # Father in jail by 1st yr
    m4i1,                             # Moved?
    m4i1a,                            # How many times moved
    m4j5:m4j17,                       # Mother depression
    m4j25a1:m4j25a2,                  # Mother impulsivity
    cm4md_case_lib,                   # Mother depression criteria
    cf4edu,                           # Father education
    cm4edu,                           # Mother education
    cm4hhinc,                         # Household income
    cm4povco,                         # Poverty threshold
    cm4adult,                         # Household composition
    cm4kids,                          # Household composition
    p4b11,                            # Routines
    p4b13,
    p4b15,
    p4b16,
    p4b18,
    p4b19,
    p4b16,
    p4c1a:p4c12,                      # Nr of toys / books
    p4g1:p4g19,                       # Discipline
    p4h1:p4h7,                        # Exposure to violence
    m4b4b1:m4b29a19,                  # CBCL + ASBI : mother
    p4l1:p4l66,                       # CBCL + ASBI : father
    o4t1:o4t17,                       # Observed parent-child interactions
    ch4cbmi,                          # Child BMI
    ch4emp_totjob,                    # Nr of jobs
    ch4cc_totarr,                     # Nr of arrangements

    # Wave 5

    m5a2,                             # Child live with mother?
    p5h1,                             # Child health
    cm5ffevjail,                      # Father in jail by 1st yr
    m5f1,                             # Moved?
    m5f1a,                            # How many times moved
    m5g3:m5g15,                       # Mother depression
    cm5md_case_lib,                   # Mother depression criteria
    cf5edu,                           # Father education
    cm5edu,                           # Mother education
    cm5hhinc,                         # Household income
    cm5povco,                         # Poverty threshold
    p5q3a:p5q3do,                     # CBCL + ASBI
    ch5cbmi,                          # Child BMI
    p5i22a:p5i22e,                    # CHAOS
    k5b1a:k5b1d,                      # Mother discipline
    k5g2a:k5g2n,                      # SDQ

    # Wave 6

    cp6ylpcg,                         # Lives with PCG?                         recode 0-1
    p6e1,                             # Rel. with other parent
    p6j1,                             # Moved?
    p6j2,                             # How many times moved
    p6h22:p6h36,                      # PCG depression
    p6h56:p6h73,                      # PCG anxiety
    cp6gad_9y,                        # PCG anxiety criteria
    cp6md_case_lib_9y,                # PCG depression criteria
    cp6edu,                           # PCG education
    cp6hhinc,                         # PCG household income
    cp6povco,                         # PCG poverty threshold
    k6d1a:k6d1l,                      # ASBI + SSRS
    ck6cbmi,                          # Child BMI
    k6c4a:k6c4e,                      # CHAOS
    k6c9a:k6c9d,                      # PCG discipline
    
    # Self-protection
                # School threats
    k6b1d,                            # I feel safe at my school = R
    k6b2,                             # Police officer or officers regularly stationed at school?
    k6b3,                             # Some other security guard regularly stationed at school?
    k6b21d,                           # Trouble getting along with other students
    k6b32a,                           # Kids at school pick on you or say mean things to you
    k6b32b,                           # Kids at school hit you or threaten to hurt you physically?
    k6b32c,                           # Kids at school help you with a problem? = R
    k6b32d,                           # Kids at school take your side of an argument? = R
    k6b32e,                           # Kids at school take things, like your money or lunch, without asking?
    k6b32f,                           # Kids at school purposely leave you out of activities?
    
                # Neighborhood threats
    k6e4b,                            # I feel unsafe walking around my neighborhood during the day
    k6e4c,                            # I feel unsafe walking around my neighborhood at night
    k6e5,                             # Ever witnessed a crime or known about a crime?
                # Family threats
    k6c5,                             # Firearms kept in or around your home?
    k6c8,                             # How often spend time alone in home without adult present?
    k6c9b,                            # PCG took away privileges or grounded you
    k6c9c,                            # PCG shouted, yelled, screamed, swore or cursed at you
    k6c9d,                            # PCG hit or slapped you
      
                # Protection from PCG and friends
    k6c6a,                            # PCG knows what you do during your free time = R
    k6c6b,                            # PCG knows what you spend money on = R
    k6c7a,                            # Decides how late you can stay out at night = R
    k6c7b,                            # Decides what kinds of TV shows/movies you can watch = R
    k6c7c,                            # Decides who you can hang out with = R
    k6c9a,                            # PCG explained to you why something you did was wrong = R
    k6c9e,                            # PCG listened to your side of an argument = R
    k6c9f,                            # PCG missed events or activities that were important to you
    k6d2l,                            # There are people in my life who really care about me = R
    k6d2g,                            # I have friends that I really care about = R
    k6d2y,                            # When I have a problem, I have someone who will be there for me = R
    
    # Affiliation
                # Family bonds
    k6c17,                            # How close do you feel with biological mother?                               # Attachment to biological parents
    k6c18,                            # How well do you and your mom share ideas/talk?                              # Attachment to biological parents
    k6c28,                            # How close do you feel with biological father?                               # Attachment to biological parents
    k6c29,                            # How well do you and biological father share ideas/talk?                     # Attachment to biological parents
    k6c33,                            # How close do you feel with mother's partner?                                # Attachment to step parents
    k6c34,                            # How well do you and mother's partner share ideas/talk?                      # Attachment to step parents 
    k6c37,                            # How close do you feel with father's partner?                                # Attachment to step parents 
    k6c38,                            # How well do you and your father's partner share ideas/talk?                 # Attachment to step parents
    k6c40,                            # Another adult in life with whom you share ideas/talk?
    k6c41,                            # How is adult with whom you share ideas/talk related to you?
    k6c45,                            # How well do you and your siblings get along?                                # Attachment to siblings
    k6c46,                            # How often jealous about mother's attention toward siblings?                 # Insecure attachment
    k6c47,                            # How often jealous about father's attention toward siblings?                 # Insecure attachment
    k6c48,                            # How often jealous about mother's partner's attention toward siblings?       # Insecure attachment
    k6c49,                            # How often jealous about father's partner's attention toward siblings?       # Insecure attachment

    #             # Need for belongingness / social approval
    k6b1a,                            # I feel close to people at my school = R
    k6b1b,                            # I feel like I am part of my school = R
    k6d2af,                           # When something good happens to me, I have people to share news with
    p6d21a,                           # Youth has friends

                  # Coalitional identity
    # Other delinquent behaviours and problems with police/justice
    k6d61a,                             # Painted graffiti or signs on private property/public spaces
    k6d61b,                             # Deliberately damaged property that didn't belong to you
    k6d61c,                             # Taken something from a store without paying for it
    k6d61d,                             # Gotten into a serious physical fight
    k6d61e,                             # Hurt someone badly enough to need bandages or medical care
    k6d61f,                             # Driven a car without its owner's permission
    k6d61g,                             # Stolen something worth more than $50
    k6d61h,                             # Gone into a house or building to steal something
    k6d61i,                             # Used or threaten to use a weapon to get something
    k6d61j,                             # Sold marijuana or other drugs
    k6d61k,                             # Stolen something worth less than $50
    k6d61l,                             # Taken part in a group fight
    k6d61m,                             # Were you loud, rowdy, or unruly in a public place
    p6b69,                              # Youth ever been stopped by police?
    p6b70,                              # Youth ever been arrested?
    p6b71,                              # Youth placed in juvenile detention prior to family court?
    p6b72,                              # Youth sentenced by judge to placement in a juvenile detention?

    # Friends delinquent behaviour
    k6d62a,                           # Friends smoked an entire cigarette                                        k6d40
    k6d62b,                           # Friends drank alcohol more than two times without their parents           k6d48
    k6d62c,                           # Friends tried marijuana
    k6d62d,                           # Friends tried other drugs to get high
    k6d62e,                           # Friends asked to go drinking with them
    k6d62f,                           # Friends given or sold marijuana to you
    k6d62g,                           # Friends deliberately damaged property that did not belong to them         k6d61b
    k6d62h,                           # Friends stole something worth more than $50                               k6d61c
    k6d62i,                           # Friends used or threatened to use a weapon to get something               k6d61i
    k6d62j,                           # Friends sold marijuana or other drugs                                     k6d61j
    k6d62k,                           # Friends stole something worth less than $50                               k6d61k
    k6d63,                            # How many of your friends do you think have had sex?
    
    # Social motivation / Esteem
    k6d1c,                            # I am open and direct about what I want                                         recode 1-3 by 3-1        
    k6d1d,                            # I join group activities without being told to                                  recode 1-3 by 3-1
    k6d1e,                            # I make friends easily                                                          recode 1-3 by 3-1
    k6d1f,                            # I am self-confident in social situations                                       recode 1-3 by 3-1
    k6d1i,                            # I start conversations rather than waiting for others to talk first             recode 1-3 by 3-1
    k6d1j,                            # I am liked by others                                                           recode 1-3 by 3-1
    k6d1k,                            # I invite others to my home                                                     recode 1-3 by 3-1

    # CBCL
    p6b35,                            # Youth is cruel, bullies, or shows meanness to others    
    p6b36,                            # Youth cries a lot    
    p6b37,                            # Youth destroys things belonging to family or others
    p6b38,                            # Youth is disobedient at home
    p6b39,                            # Youth is disobedient at school
    p6b40,                            # Youth feels worthless or inferior
    p6b41,                            # Youth gets in many fights
    p6b42,                            # Youth physically attacks people
    p6b43,                            # Youth is stubborn, sullen, or irritable
    p6b44,                            # Youth has temper tantrums or a hot temper
    p6b45,                            # Youth threatens people
    p6b46,                            # Youth can't concentrate, can't pay attention for long
    p6b47,                            # Youth can't sit still, is restless or hyperactive
    p6b48,                            # Youth is impulsive or acts without thinking
    p6b49,                            # Youth doesn't seem to feel guilty after misbehaving
    p6b50,                            # Youth hangs around with others who get in trouble
    p6b51,                            # Youth lies or cheats
    p6b52,                            # Youth is nervous, highstrung, or tense
    p6b53,                            # Youth is too fearful or anxious
    p6b54,                            # Youth feels too guilty
    p6b55,                            # Youth has trouble sleeping
    p6b56,                            # Youth clings to adults or too dependent
    p6b57,                            # Youth is unusually loud
    p6b58,                            # Youth talks too much
    p6b59,                            # Youth argues a lot
    p6b60,                            # Youth runs away from home
    p6b61,                            # Youth sets fires
    p6b62,                            # Youth steals at home
    p6b63,                            # Youth steals outside the home
    p6b64,                            # Youth swears or uses obscene language
    p6b65,                            # Youth is underactive, slow moving, or lacks energy
    p6b66,                            # Youth is unhappy, sad, or depressed
    p6b67,                            # Youth vandalizes
    p6b68,                            # Youth worries

    # Anxiety/Depression 
    k6d2b,                              # I love life
    k6d2c,                              # I feel I cannot shake off the blues, even with help > Pervasive influence of feelings
    k6d2d,                              # I have spells of terror or panic                    > Pervasive influence of feelings 
    k6d2j,                              # I feel tense or keyed up                            
    k6d2t,                              # I get suddenly scared for no reason                 > Pervasive influence of feelings
    k6d2ag,                             # I feel nervous or shaky inside                      

    # Shoter-term mindset components
    k6d2a,                            # I don't spend enough time thinking over a situation before I act      > Urgency to act R
    k6d2p,                            # I often say and do things without considering the consequences        > Urgency to act R
    k6d2z,                            # I often make up my mind without taking the time to consider           > Urgency to act R
    k6d2ab,                           # I often say whatever comes into my head without thinking first        > Urgency to act R
    k6d2aj,                           # I often get into trouble because I don't think before I act           > Urgency to act R
    k6d2r,                            # The plans I make don't work out because I haven't gone over them      > Urgency to act R
    k6d2k,                            # Once I make a plan to get something done, I stick to it               > Lack of perseverance           
    k6d2m,                            # I finish whatever I begin                                             > Lack of perseverance           
    k6d2i,                            # I keep at my schoolwork until I am done with it                       > Lack of perseverance           
    k6d2v,                            # I am a hard worker                                                    > Lack of perseverance           
    k6d2e,                            # I get so involved in activities that I forget about everything else   > Distractibility            
    k6d2h,                            # I get completely absorbed in what I am doing                          > Distractibility            
    k6d2u,                            # When I do an activity, I enjoy it so much that I lose track of time   > Distractibility            
    k6d2ad,                           # When I am learning something new, I lose track of time                > Distractibility            
    
    # Health status
    p6b1,                             # Parents' perceived health of the child
    p6b2,                             # Doctor diagnosed youth with asthma                          replace 2 by 0 (NO)
    p6b3,                             # Doctor diagnosed youth with anemia                          replace 2 by 0 (NO)
    p6b4,                             # Doctor diagnosed youth with heart disease/condition         replace 2 by 0 (NO)
    p6b5,                             # Doctor diagnosed youth with depression/anxiety              replace 2 by 0 (NO)
    p6b6,                             # Doctor diagnosed youth with diabetes                        replace 2 by 0 (NO)
    p6b7,                             # Doctor diagnosed youth with limb problems                   replace 2 by 0 (NO)
    p6b8,                             # Doctor diagnosed youth with seizures                        replace 2 by 0 (NO)
    p6b9,                             # Doctor diagnosed youth with other condition                 replace 2 by 0 (NO)
    p6b10,                            # Doctor diagnosed youth with ADD/ADHD                        replace 2 by 0 (NO)
    p6b11,                            # Doctor diagnosed youth with Autism                          replace 2 by 0 (NO)
    p6b12,                            # Doctor diagnosed youth with other learning disability       replace 2 by 0 (NO)
    k6d3,                             # Subjective general health status                           
    
    # Health efforts
    k6d37,                            # Days physically active for 60+ minutes in past week = R
    k6d38,                            # Days engage in physical activity for 30+ minutes in typical week = R
    k6d39,                            # Days participate in vigorous physical activity in typical week = R
    k6d40,                            # Ever smoked an entire cigarette?                                            replace 2 by 0 (NO)      
    k6d41,                            # Age when youth first smoked a whole cigarette (years)
    k6d42,                            # How often smoked cigarettes in past month?
    k6d43,                            # How many cigarettes per day smoked in past month?
    k6d48,                            # Ever drank alcohol more than two times without parents?                     replace 2 by 0 (NO)
    k6d49,                            # How old were you when you first drank alcohol? (years)
    k6d50,                            # How often drank alcohol in past month?
    k6d51,                            # How many alcoholic drinks had each time in past month?
    k6d52,                            # How often drank alcohol in past year?
    k6d53,                            # How many alcoholic drinks had each time in past year?
    k6d54,                            # How often drank five or more alcoholic drinks in past year?
    k6d55,                            # How often got drunk in past year?

    # Education efforts 15y
    p6c11,                            # Discussed academic problems with youth's teacher                            replace 2 by 0 (NO)
    p6c12,                            # Discussed tardiness or absences with youth's teacher                        replace 2 by 0 (NO)
    p6c13,                            # Discussed homework not done with youth's teacher                            replace 2 by 0 (NO)
    p6c23,                            # Youth required to attend summer school since last int?                      replace 2 by 0 (NO)
    p6c25,                            # Youth repeated any grades since last int?                                   replace 2 by 0 (NO)

    # Mating efforts 15y
    k6f4,                             # Ever dated?                                                                 replace 2 by 0 (NO)
    k6f5,                             # Age at 1st date
    k6f6,                             # How many dated
    k6f26,                            # Ever sex?                                                                   replace 2 by 0 (NO)
    k6f31,                            # Age at 1st sex
    k6f35,                            # How many sex
    
    # Exploratory tendencies 15y 
    k6d1g,                            # I easily change from one activity to another
    k6d1h)                            # I show interest in a variety of things                            

#### Recode variables

write.csv(mydf.raw,"mydf.raw.csv",row.names = F)

#### Some checks
# Raw percentage of missing cases
missing <- ifelse(mydf.raw < 0,1,0) %>%
  colMeans() %>%
  as.data.frame()

# Detailed info on missing cases
missing_detailed <- mapply(table,mydf.raw[,-1])

# Check number of subjects with both parental questionnaire and home observation data
testdat <- mydf.raw[,c("o3t1","p3m1")]
testdat$chk <- ifelse((testdat$o3t1 < 0) | (testdat$p3m1 < 0),0,1)
sum(testdat$chk)
mean(testdat$chk)

# Check the attachment variable distributions
atts <- gather(mydf.raw,dim,val,ch3att_b1:ch3att_b3)
ggplot(atts,aes(x = val,fill = dim)) + geom_density(alpha = 0.4) + theme_classic()

# Check overall attrition
attrition <- mydf.raw[,c("m2a3","m3a2","m4a2","m5a2","cp6ylpcg")]
attrition$chk <- ifelse((attrition$m2a3 == -9) | (attrition$m3a2 == -9) | (attrition$m4a2 == -9) | (attrition$m5a2 == -9) | (attrition$cp6ylpcg == -9),0,1)
sum(attrition$chk)
mean(attrition$chk)
