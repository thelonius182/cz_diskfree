wpdb <- get_wp_conn()
typeof(wpdb)

sql_stm <- "select * from wp_posts where post_type regexp 'programma' and post_date > '2024-01-01';"
wp_posts <- dbGetQuery(wpdb, sql_stm)
swf_posts <- wp_posts |> mutate(bc_post_id = ID,
                                bc_start = ymd_hms(post_date, tz = "Europe/Amsterdam"),
                                bc_title = post_title,
                                bc_published = if_else(post_status == "publish", T, F),
                                bc_platform = if_else(post_type == "programma", "CZ", "WJ")) |>
  select(starts_with("bc_"))

sql_stm <- "select p1.ID as bc_post_id, m1.meta_value as bc_replay_of
from wp_postmeta m1 join wp_posts p1 on p1.ID = m1.post_id
where meta_key = 'pr_metadata_orig' and length(m1.meta_value) > 0;"
wp_replays <- dbGetQuery(wpdb, sql_stm)

swf_replays <- wp_replays |> mutate(bc_replay_of = as.integer(bc_replay_of)) |>
  inner_join(swf_posts, by = join_by(bc_post_id)) |> select(bc_post_id, bc_replay_of)

sql_stm <- "select p1.ID as bc_post_id, m1.meta_value as bc_stop
from wp_postmeta m1 join wp_posts p1 on p1.ID = m1.post_id
where meta_key = 'pr_metadata_uitzenddatum_end' and length(m1.meta_value) > 0;"
wp_endings <- dbGetQuery(wpdb, sql_stm)

swf_endings <- wp_endings |> mutate(bc_stop = ymd_hm(bc_stop, tz = "Europe/Amsterdam")) |>
  inner_join(swf_posts, by = join_by(bc_post_id)) |> select(bc_post_id, bc_stop)

sql_stm <- "select object_id as post_id, x1.description as post_translations
from wp_term_relationships r1
join wp_term_taxonomy x1 on x1.term_taxonomy_id = r1.term_taxonomy_id
join wp_terms t1 on t1.term_id = x1.term_id
join wp_posts p1 on p1.ID = r1.object_id
where x1.taxonomy = 'post_translations'
and post_type regexp '^programma'
and post_date > '2024-01-01'
order by 1;"
wp_translations <- dbGetQuery(wpdb, sql_stm)

swf_translation <- wp_translations |>
  mutate(post_id_nl = str_extract(post_translations,
                                  pattern = '"nl";i:(\\d{6,});',
                                  group = 1) |> as.integer(),
         post_id_en = str_extract(post_translations,
                                  pattern = '"en";i:(\\d{6,});',
                                  group = 1) |> as.integer()) |>
  select(-post_translations)

sql_stm <- "select object_id as post_id, t1.slug as lang
from wp_term_relationships r1
join wp_term_taxonomy x1 on x1.term_taxonomy_id = r1.term_taxonomy_id
join wp_terms t1 on t1.term_id = x1.term_id
join wp_posts p1 on p1.ID = r1.object_id
where x1.taxonomy = 'language'
and post_type regexp '^programma'
and post_date > '2024-01-01'
order by 1;"
wp_language <- dbGetQuery(wpdb, sql_stm)

sql_stm <- "select object_id as post_id, t1.term_id, t1.slug as genre
from wp_term_relationships r1
     join wp_term_taxonomy x1 on x1.term_taxonomy_id = r1.term_taxonomy_id
     join wp_terms t1 on t1.term_id = x1.term_id
     join wp_posts p1 on p1.ID = r1.object_id
where x1.taxonomy = 'programma_genre'
  and t1.slug regexp '__.*-(nl|en)$'
  and post_type regexp '^programma'
  and post_date > '2024-01-01'
order by 1;
"
wp_genre <- dbGetQuery(wpdb, sql_stm)
wrk_genre_err <- wp_genre |> group_by(post_id) |> summarize(n = n()) |> filter(n > 2)

swf_title <- wp_genre |> anti_join(wrk_genre_err, by = join_by(post_id)) |>
  mutate(title_id = term_id,
         title_slug = str_extract(genre, "(.*)__.*", group = 1),
         genre_slug = str_extract(genre, "__(.*)-(nl|en)$", group = 1),
         lang = str_extract(genre, "__.*-(..)$", group = 1)) |>
  select(-term_id, -genre) |> arrange(post_id, genre_slug) |>
  group_by(post_id) %>%
  summarize(
    title_id = first(title_id),
    title_slug = first(title_slug),
    genre_slug = paste(genre_slug, collapse = "-"),
    lang = first(lang),
    .groups = "drop"
  )

sql_stm <- "select object_id as post_id, t1.term_id, t1.slug as editor
from wp_term_relationships r1
     join wp_term_taxonomy x1 on x1.term_taxonomy_id = r1.term_taxonomy_id
     join wp_terms t1 on t1.term_id = x1.term_id
     join wp_posts p1 on p1.ID = r1.object_id
where x1.taxonomy = 'programma_maker'
  and post_type regexp '^programma'
  and post_date > '2024-01-01'
order by 1;
"
wp_editor <- dbGetQuery(wpdb, sql_stm)
wrk_editor_err <- wp_editor |> group_by(post_id) |> summarize(n = n()) |> filter(n > 4)
swf_editor <- wp_editor |> anti_join(wrk_editor_err, by = join_by(post_id)) |>
  mutate(editor_id = term_id,
         editor_slug = str_extract(editor, "(.*)-(nl|en)$", group = 1),
         lang = str_extract(editor, ".*-(..)$", group = 1))
