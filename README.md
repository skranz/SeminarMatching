## Seminar Matching (in development)

Shiny based R package to perform centralized matching of students to seminars.

- The deployed software consists of several web apps:

    + for lecturers to enter and specify seminars and see results
  
    + for students to enter background data, rank seminars and see results
  
    + for admins to specify general parameters like the date of seminar matching and see general report.

+ Students will be matched via the Gale-Shapley student optimal mechanismen.
+ There is a random component to students' priorities, but lecturers have also the option to give priority over students, e.g. based on already heard courses, the number of semesters or fields of specialization.

+ The package can relatively flexibly be adapted, via yaml parameters and markdown forms and reports. But documentation is so far very poor.

## Timeline of seminar matching for a semester

- Adminstrator specifies all relevant dates for a given semester. This can be done quite some time ahead. The dates are saved in the `admin` database table.

- `default_start_date`: From this date on, the seminar will be the default seminar in teachers' and students' web apps.

- Until one day before `stud_start_date`: Lecturers can enter and activate seminars for the current semester.
    - Somebody should send reminder emails at `default_start_date` and one week before `stud_start_date`.

- From `stud_start_date` to `matching1_start_date-1` students should enter their (ordinal) preferences over seminars.
    - Lecturers can still change seminar settings like the number of slots, or weblinks. They should avoid to deactivate seminars, however.

- On `matching1_start_date` global random priority points `r_i` will be drawn and from a uniform distribution between 0 and 10 and stored for each student $i$.
    - Extension: One can customize the software such that students who add a sufficiently large number `n_i` of seminars to their preference list, are 'insured' against too low priorities. The actual random priority points will be `max(r_i, r_min[n_i])`, where `r_min[n]` is increasing in the number of selected seminars. The idea is that those students who add a large list of seminars really need a seminar, and should be likely to get a slot. More precisely, we want to make it likely that students who are willing to take say 50% of all offered seminars, get a slot.
    
        Truthful revelation is partly destroyed by such an extension. Students may be incentivized to add longer seminar lists than they actually want. Yet, if we can impose some penalty if students don't take an assigned seminar, we may counterveil such incentives. Fortunately, there are still no incentives to modify the ranking of seminars. 

- From `matching1_start_date` to `matching1_date-1`, lecturers see for each of their seminars a list of all students that put that seminar in their preference ordering. The students are ordered by their automatically computed priorities. Lecturers can adapt students' priorities and may also increase the number of seminar slots.
    - Lecturers see background data of students, like subject, specialization or taken courses
    - Lecturers DON'T see how a student has ranked their seminar (otherwise truthful ranking may no longer be incentive compatible for students, given that lecturers may adapt their priorities based on a student's ranking).
    - Lecturers also see some aggregate statistics that may help their decision of whether to expand their number of seminar slots:
        - How many students chose their seminar as 1st, 2nd or 3rd choice.
        - How many students would be matched if matching took place with current priorities.

- On `matching1_date` the actual seminar matching will take place (with a chromtab job or the admin) using the updated priorities. After the matching has been conducted, students and lecturers can see their results.

- From `matching1_date` to `matching2_start_date-1` students can enter their priorities over seminars for the 2nd matching round.

- The second matching round then proceeds in a similar spirit than the first matching round. Yet, there are no particular information shown that is relevant for increasing the number of slot. Slot increases should be done in the first round.

- After both matching rounds are conducted, lecturers can contact students for a meeting to assign topics. Topic assignment is not yet part of this software.


### Further aspects

### Negative Autocorrelation in Random Priority Points

Often students have to participate in more than one seminar. It seems a bit unfair, if one student draws in every semester a low random priority. To counterveil this effect, we can include some negative autocorrelation between random priorities. If you had a high priority last time, you are more likely to get low priority this time, and vice versa. The relant lines of codes are the following:

```
   # give a bonus if last time random points were below 5
    if (stud$random_points < 5) {
      stud$random_points = runif(1,5-stud$random_points,10)
    # give a malus if last time random points were above 5
    } else {
      stud$random_points = runif(1,0,15-stud$random_points)
    }
```
