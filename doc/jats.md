---
title: JATS
author: Albert Krewinkel
---

This document describes pandoc's handling of JATS.

Metadata Values
===============

`abstract`
:   Article summary. Added via the document's front matter via the
    [`<abstract>`][elem:abstract] element.

`author`
:   list of article contributors. Each author should have a surname
    and a given name listed in the entry; if the author has no
    `surname` value, then the item will be used as the contributors
    [`string-name`][elem:string-name].

    `orcid`
    :   the contributor's ORCID identifier.

    `surname`
    :   surname of the contributor. Usually the family name in
        western names.

        See [`<surname>`][elem:surname].

    `given-names`
    :   personal names of the contributor; this includes middle
        names (if any) in western-style names.

        See [`<given-names>`][elem:given-names].

    `name`
    :   full name of the author; included only as a fallback if
        `author.surname` is not available. Tagged with
        [`<string-name>`][elem:string-name].

    `email`
    :   the contributor's email address.

        Used as the contents of the [`<email>`][elem:given-names]
        element.

    `affiliation`
    :   list of affiliation identifiers; marks the organizations
        with which an author is affiliated. Each identifier in this
        list must also occur as the `id` of an affiliation listed in
        the top-level `affiliation` list.

    `equal-contrib`
    :   boolean attribute used to mark authors who contributed
        equally to the work. The
        [`equal-contrib`][attr:equal-contrib] attribute is added
        to the author's [`<contrib>`] element if this is set to a
        truthy value.

    `cor-id`
    :   identifier linking to the contributor's correspondence
        information. The info itself must be stored in as an item in
        `article.author-notes.corresp`. If the `cor-id` value is
        then, an [`<xref>`][elem:xref] link of
        [`ref-type`][attr:ref-type] `corresp` is added. The
        [`rid`][attr:rid] attribute is set to `cor-<ID>`, where
        `<ID>` is the stringified value of this attribute.

`affiliation`
:   the list of organizations with which contributors are
    affiliated. Each institution is added as an [`<aff>`] element to
    the author's contrib-group.

    The fields are given in the order in which they are included in
    the output.

    `id`
    :   internal identifier; used as the [`<aff>`] element's `id`
        value, prefixed with `aff-`.

    `group`
    :   name of the research group or other low-level organizational
        structure; used as value of an [`<institution>`] element with
        [`content-type`][attr:content-type] set to `group`.

    `department`
    :   name of the department or other mid-level organizational
        structure; used as value of an [`<institution>`] element with
        [`content-type`][attr:content-type] set to `dept`.

    `organization`
    :   name of the company, university, or other top-level
        organizational structure; used as value of an
        [`<institution>`] element. The institution element is wrapped
        in an [`<institution-wrap>`] element; any identifiers, like
        `ringgold` or `ror`, are added to the wrapper and must hence
        belong to this organization (not the department or group).

    `isni`
    :   International Standard Name Identifier of the organization.
        Added via an [`<institution-id>`] element with
        [`institution-id-type`](attr:institution-id-type) set to
        `ISNI`.

    `ringgold`
    :   [Ringgold] identifier of the organization. Added via an
        [`<institution-id>`] element with
        [`institution-id-type`](attr:institution-id-type) set to
        `Ringgold`.

    `ror`
    :   Research Organization Registry identifier of the
        organization. Added via an [`<institution-id>`] element with
        [`institution-id-type`](attr:institution-id-type) set to
        `ROR`.

    `pid`
    :   Array of persistent identifiers which are added as
        [`<institution-id>`] elements. Each item must contain a map
        with keys `type`, used as
        [`institution-id-type`](attr:institution-id-type), and `id`,
        used as element content.

    `street-address`
    :   The organization's street address; each list item is wrapped
        in an [`<addr-line>`] element, separated by a comma and
        space (`, `).

    `city`
    :   City in which the organization is located; used only if
        `street-address` is not given, in which case the value is
        wrapped in a [`<city>`] element.

    `country`
    :   Country in which the organization is located; used as the
        value of a [`<country>`] element.

    `country-code`
    :   Two letter ISO-3166-1 country identifier; used as the
        [`country`][attr:country] attribute in element [`<country>`]
        (if the latter is present).

`copyright`
:   Licensing and copyright information. This information is
    rendered via the [`<permissions>`][elem:permissions] element.

    The variables `type`, `link`, and `text` should always be used
    together.

    `statement`
    :   the year of copyright; used as content of the
        [`<copyright-statement>`][elem:copyright-statement]

    `year`
    :   the year of copyright; used as content of the
        [`<copyright-year>`][elem:copyright-year]

    `holder`
    :   the copyright holder; included via the
        [`<copyright-holder>`][elem:copyright-holder] element.

    `text`
    :   inline text setting the license under which the text is
        published; included via the
        [`<license-p>`][elem:copyright-holder] element.

    `type`
    :   type of the license; used as value of the
        [`license-type`][attr:license-type] attribute.

    `link`
    :   external link describing the license; used as value of a
        `xlink:href` attribute in the `<license>` element.

`date`
:   publication date. This value should usually be a string
    representation of a date. Pandoc will parse and deconstruct the
    date into the components given below. It is also possible to
    pass these components directly.

    The publication date is recorded in the document via the
    [`<pub-date>`] element and its sub-elements. The
    [`publication-format`][attr:publication-format] attribute is
    always set to `electronic`.

    `iso-8601`
    :   ISO-8601 representation of the publication date. Used as the
        value of the [`<pub-date>`] element's
        [`iso-8601-date`][attr:iso-8601-date] attribute.

        This value is set automatically if pandoc can parse the
        `date` value as a date.

    `day`, `month`, `year`
    :   Day, month, and year of the publication date. Only the
        publication year is required. The values are used as the
        contents of the elements with the respective names.

        The values are set automatically if pandoc can parse the
        `date` value as a date.

    `type`
    :   The type of event marked by this date. The value is set as
        the [`date-type`][attr:date-type] attribute on the
        [`<pub-date>`] element and defaults to "pub" if not
        specified.

`article`
:   information concerning the article that identifies or describes
    it. The key-value pairs within this map are typically used
    within the [`<article-meta>`][elem:article-meta] element.

    `publisher-id`
    :   external article identifier assigned by the publisher. Used
        as the content of the [`<article-id>`][elem:article-id]
        element with attribute [`pub-id-type`][attr:pub-id-type] set
        to `publisher-id`.

    `doi`
    :   Digital Object Identifier (DOI) assigned to the article.
        Used as the content of the [`<article-id>`][elem:article-id]
        element with attribute [`pub-id-type`][attr:pub-id-type] set
        to `doi`.

    `pmid`
    :   PubMed Identifier (PubMed ID) assigned to the article. Used
        as the content of the [`<article-id>`][elem:article-id]
        element with attribute [`pub-id-type`][attr:pub-id-type] set
        to `pmid`.

    `pmcid`
    :   PubMed Central Identifier assigned to the article. Used as
        the content of the [`<article-id>`][elem:article-id] element
        with attribute [`pub-id-type`][attr:pub-id-type] set to
        `pmcid`.

    `art-access-id`
    :   generic article accession identifier. Used as the content of
        the [`<article-id>`][elem:article-id] element with attribute
        [`pub-id-type`][attr:pub-id-type] set to `art-access-id`.

    `heading`
    :   name of a subject or topic describing the article. Used as
        the content of the [`<subject>`][elem:subject] element,
        nested in a [`<subj-group>`][elem:subj-group] element which
        has `heading` as its
        [`subj-group-type`][attr:subj-group-type] attribute.

    `categories`
    :   list a subject or topic describing the article. Items are
        each used as the content a the [`<subject>`][elem:subject]
        element, grouped in a single
        [`<subj-group>`][elem:subj-group] element with its
        [`subj-group-type`][attr:subj-group-type] attribute set to
        `categories`.

    `author-notes`
    :   Additional information about authors, like conflict of
        interest statements and corresponding author contact info.
        Wrapped in an [`<author-notes>`][elem:author-notes] element.

        `conflict`
        :   Conflict of interest statement. Rendered as a footnote
            ([`<fn>`][elem:fn]) of [`fn-type`](attr:fn-type)
            `conflict`.

        `con`
        :   Contributed-by information. Rendered as a footnote
            ([`<fn>`][elem:fn]) of [`fn-type`](attr:fn-type) `con`.

        `corresp`
        :   Correspondence information. This must be a list of
            contributor correspondence items, where each item must
            have the properties `id` and `email`. The info is then
            rendered via a [`<corresp>`][elem:corresp] element.

    `funding-statement`
    :   Prose describing the funding. Added to the article's
        frontmatter via the
        [`funding-statement`][elem:funding-statement] element.

`journal`
:   information on the journal in which the article is published.
    This must be a map; the following key/value pairs are
    recognized.

    `publisher-id`
    :   journal identifier assigned by the publisher. Used as
        content of element [`<journal-id>`][elem:journal-id] with
        attribute [`journal-id-type`][attr:journal-id-type] set to
        `publisher-id`.

    `nlm-ta`
    :   journal identifier assigned by PubMed. Used as content of
        element [`<journal-id>`][elem:journal-id] with attribute
        [`journal-id-type`][attr:journal-id-type] set to `nlm-ta`.

    `pmc`
    :   journal identifier assigned by PubMed Central. Used as
        content of element [`<journal-id>`][elem:journal-id] with
        attribute [`journal-id-type`][attr:journal-id-type] set to
        `pmc`.

    `title`
    :   full title of the journal in which the article is published.
        Used as content of the
        [`<journal-title>`][elem:journal-title] element.

    `abbrev-title`
    :   short form of the journal title. Used as content of the
        [`<abbrev-journal-title>`][elem:abbrev-journal-title]
        element.

    `pissn`
    :   ISSN identifier of the publication's print version. Used as
        content of the [`<issn>`][elem:issn] element with the
        [`publication-format`][attr:publication-format] attribute
        set to `print`.

    `eissn`
    :   ISSN identifier of the publication's electronic version.
        Used as content of the [`<issn>`][elem:issn] element with
        the [`publication-format`][attr:publication-format]
        attribute set to `electronic`.

    `publisher-name`
    :   name of the publishing entity (person, company, or other).
        Used as the content of the
        [`<publisher-name>`][elem:publisher-name] element.

    `publisher-loc`
    :   place of publication. Used as the content of the
        [`<publisher-loc>`][elem:publisher-loc] element.

`notes`
:   Additional notes concerning the whole article. Added to the
    article's frontmatter via the [`<notes>`][elem:notes] element.

`tags`
:   list of keywords. Items are used as contents of the
    [`<kwd>`][elem:kwd] element; the elements are grouped in a
    [`<kwd-group>`][elem:kwd-group] with the
    [`kwd-group-type`][attr:kwd-group-type] value `author`.

Required Metadata
-----------------

Pandoc will try to generate a valid JATS document even when
information is missing, filling in placeholders or using empty
values. This circumvents the intend to ensure a minimum set of
information being present in documents of a certain tag set. It is
hence recommended to always provide the information listed below.

### Publishing Tag Set

Required metadata values:

- One or more of `journal.publisher-id`, `journal.nlm-ta`,
  `journal.pmc`.
- One or more of `journal.pissn`, `journal.eissn`.

[Ringgold]: https://ringgold.com/
[attr:content-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/content-type.html
[attr:date-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/date-type.html
[attr:equal-contrib]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/equal-contrib.html
[attr:fn-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/fn-type.html
[attr:institution-id-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/institution-id-type.html
[attr:iso-8601-date]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/iso-8601-date.html
[attr:journal-id-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/journal-id-type.html
[attr:kwd-group-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/kwd-group-type.html
[attr:license-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/license-type.html
[attr:pub-id-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/pub-id-type.html
[attr:publication-format]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/publication-format.html
[attr:ref-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/ref-type.html
[attr:rid]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/rid.html
[attr:subj-group-type]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/attribute/subj-group-type.html
[elem:abbrev-journal-title]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/abbrev-journal-title.html
[elem:abstract]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/abstract.html
[elem:article-id]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/article-id.html
[elem:article-meta]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/article-meta.html
[elem:copyright-holder]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/copyright-holder.html
[elem:copyright-statement]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/copyright-statement.html
[elem:copyright-year]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/copyright-year.html
[elem:corresp]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/corresp.html
[elem:email]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/email.html
[elem:fn]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/fn.html
[elem:funding-statement]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/funding-statement.html
[elem:given-names]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/given-names.html
[elem:issn]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/issn.html
[elem:journal-id]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/journal-id.html
[elem:journal-title]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/journal-title.html
[elem:kwd-group]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/kwd-group.html
[elem:kwd]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/kwd.html
[elem:license-p]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/license-p.html
[elem:license]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/license.html
[elem:notes]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/notes.html
[elem:permissions]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/permissions.html
[elem:publisher-loc]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/publisher-loc.html
[elem:publisher-name]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/publisher-name.html
[elem:string-name]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/string-name.html
[elem:subj-group]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/subj-group.html
[elem:subject]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/subject.html
[elem:surname]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/surname.html
[elem:xref]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/xref.html

[`<addr-line>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/addr-line.html
[`<aff>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/aff.html
[`<city>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/city.html
[`<contrib>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/contrib.html
[`<country>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/country.html
[`<institution-id>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/institution-id.html
[`<institution-wrap>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/institution-wrap.html
[`<institution>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/institution.html
[`<pub-date>`]: https://jats.nlm.nih.gov/publishing/tag-library/1.2/element/pub-date.html
