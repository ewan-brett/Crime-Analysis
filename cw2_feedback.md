# Coursework 2 Feedback

**Student:** Ewan Brett
**Mark:** `68%`

## Overall summary
This submission was judged to be a competent analysis with only minor gaps overall. The points below summarise the main strengths, areas for improvement, and next steps used to support the final judgement.

## Strengths
- Used point pattern analysis (kernel density) to identify hotspot in north-east
- Performed PCA on ward profiles with scaling, interpreted PC1 loadings
- Recommendations are specific and backed by quantitative evidence (e.g., 86.5% of incidents in distant wards)
- Deliberately used coordinate-based ward assignment rather than the recorded labels, after identifying 39 mislabelled incidents — a sound, well-justified data-quality choice.

## Areas for improvement
- Raw incident counts were used too heavily in the explanatory stage, without a clearer incident rate or other area/population-adjusted burden measure, so the later comparisons risked overstating burden in larger wards.
- The PCA route identified a plausible deprivation structure, but the link from that structure to the incident outcome still needed a tighter quantitative explanation.
- This remained a taught PCA route overall, but the method structure did not remove the need for a more defensible incident burden measure and clearer multivariate explanation before turning the analysis into final priorities.
- Some of the recommendation logic remained exposed to confounding, because deprivation, distance, and related factors were not separated clearly enough.

## Overall analytical comment
- The data preparation work and the later analytical work were considered together when forming the final judgement.
- Missing values dropped in PCA reduced number of wards, but impact likely limited; duplicate incidents not checked could inflate counts but unlikely to change pattern.
- The overall mark reflects both the analytical substance of the work and the quality of the evidence used to justify the conclusions.

## What to improve next time
- Use a better-normalised incident outcome before drawing ward-level explanatory comparisons.
- When using PCA, show more clearly how the component pattern translates into the final intervention logic.
- Make the handling of missing profile values and duplicate checks more explicit before turning the analysis into final priorities.

## Presentation notes
No ward labels on some maps! A `geom_smooth()` formula message was left visible in Exhibit 4; suppress these messages in the final report.

---

For the general cohort-level feedback on Coursework 2, see the course website:

<https://github.bath.ac.uk/pages/ma22019-2026/ma22019_website/coursework_02_cohort_feedback.html>
