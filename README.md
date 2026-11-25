# TCJC website

Quarto site for the Texas Crime and Justice Center.

- **Edit content** in `data/*.yml` (people, projects, grants, advisory board). Pages read these files; never type a person, project, or number into a page.
- **Headshots** go in `images/people/`; put the file name in that person's `photo:` field.
- **Preview:** `quarto preview` from this folder. **Build:** `quarto render` writes `_site/`.
- Look and feel: `theme.scss`. Navigation and footer: `_quarto.yml`.
- Hosting (GitHub org `tcjc` + GitHub Pages) is still to be set up.
