library(dutchtabletopsdata)
library(purrr)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(ggrepel)
library(forcats)
library(randomForest)
library(broom)
library(gganimate)
library(tweenr)
library(stringr)
library(lumberjack)


# Model artists with only n or more paintings

n_ptg <- 5
dt_reveleved <- dt_compiled %>%
  mutate(
    artist_relationship = recode_factor(artist_relationship, "Co-painter" = "After", "Possible" = "After"),
    artist_union = as.factor(paste(artist, artist_relationship, sep = " - ")))

cleared_artists <- unique(filter(group_by(dt_reveleved, artist), n() >= n_ptg)$artist)
cleared_artist_union <- unique(filter(group_by(dt_reveleved, artist_union), n() >= n_ptg)$artist_union)

dt_valid <- dt_reveleved %>%
  filter(artist %in% cleared_artists, artist_union %in% cleared_artist_union) %>%
  select(one_of(comp_vars(), motif_vars(), "year_early", "year_late", "artist", "artist_union", "artist_relationship")) %>%
  mutate(year = (year_early + year_late) / 2, artist_union = fct_drop(artist_union)) %>%
  dmap_if(function(x) is.character(x) | is.logical(x), as.factor)

# Simple count ----
ggplot(dt_valid, aes(x = artist, fill = artist_relationship)) +
  geom_bar() +
  coord_flip()


partition <- function(df, response_var, ratio = 0.2) {
  n_obs <- nrow(df)
  all_resp <- df[[response_var]]
  all_data <- df %>%
    select(one_of(comp_vars(), motif_vars())) %>%
    na.roughfix()
  test_index <- sample(seq_len(n_obs), size = ratio * n_obs, replace = FALSE)
  test_data <- all_data[test_index,]
  test_resp <- all_resp[test_index]
  train_data <- all_data[-test_index,]
  train_resp <- all_resp[-test_index]

  def_index <- df[["artist_relationship"]] == "Definite"
  def_data <- all_data[def_index,]
  def_res <- all_resp[def_index]
  unk_data <- all_data[!def_index,]
  unk_res <- all_resp[!def_index]

  l <- list(
    allx = all_data,
    ally = all_resp,
    defx = def_data,
    defy = def_res,
    unkx = unk_data,
    unky = unk_res,
    x = train_data,
    y = train_resp,
    xtest = test_data,
    ytest = test_resp)

  if (is.numeric(train_resp)) {
    quant_breaks <- quantile(train_resp, probs = seq(0, 1, length.out = 5))
    quant_labels <- quant_breaks[1:4]

    l$yquant = cut(train_resp, breaks = quant_breaks, labels = quant_labels)
    l$minsamp = round(n_obs/5) - 1
  } else {
    l$yquant = train_resp
    l$minsamp = last(fct_count(train_resp, sort = TRUE)[["n"]])
  }

  l
}

# artist_data <- partition(dt_valid, "artist")
year_data <- partition(dt_valid, "year")
artist_union_data <- partition(dt_valid, "artist")


year_forest <- randomForest(x = year_data$defx, y = year_data$defy, xtest = year_data$unkx, ytest = year_data$unky, ntree = 500, importance = TRUE, keep.forest = TRUE)

# artist_motif_forest <- randomForest(x = select(artist_data$allx, one_of(motif_vars())), y = artist_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# artist_comp_forest <- randomForest(x = select(artist_data$allx, one_of(comp_vars())), y = artist_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# artist_all_forest <- randomForest(x = artist_data$allx, y = artist_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
#
#
#
# artist_union_motif_forest <- randomForest(x = select(artist_union_data$allx, one_of(motif_vars())), y = artist_union_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# artist_union_comp_forest <- randomForest(x = select(artist_union_data$allx, one_of(comp_vars())), y = artist_union_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)
# artist_union_all_forest <- randomForest(x = artist_union_data$allx, y = artist_union_data$ally, ntree = 500, importance = TRUE, proximity = TRUE, keep.forest = TRUE)

artist_motif_forest <- randomForest(
  x = select(artist_union_data$defx, one_of(motif_vars())),
  y = artist_union_data$defy,
  xtest = select(artist_union_data$unkx, one_of(motif_vars())),
  ytest = artist_union_data$unky,
  localImp = TRUE,
  proximity = TRUE,
  keep.forest = TRUE,
  strata = artist_union_data$defy,
  sampsize = 100,
  replace = TRUE,
  ntree = 10000)

artist_comp_forest <- randomForest(
  x = select(artist_union_data$defx, one_of(comp_vars())),
  y = artist_union_data$defy,
  xtest = select(artist_union_data$unkx, one_of(comp_vars())),
  ytest = artist_union_data$unky,
  localImp = TRUE,
  proximity = TRUE,
  keep.forest = TRUE,
  strata = artist_union_data$defy,
  sampsize = 100,
  replace = TRUE,
  ntree = 10000)

artist_all_forest <- randomForest(
  x = select(artist_union_data$defx, one_of(motif_vars(), comp_vars())),
  y = artist_union_data$defy,
  xtest = select(artist_union_data$unkx, one_of(motif_vars(), comp_vars())),
  ytest = artist_union_data$unky,
  localImp = TRUE,
  proximity = TRUE,
  keep.forest = TRUE,
  strata = artist_union_data$defy,
  sampsize = 100,
  replace = TRUE)

error <- function(rf, test = FALSE) {
  if (test) {
    er <- rf[["test"]][["err.rate"]]
  } else {
    er <- rf[["err.rate"]]
  }

  er %>%
    as.data.frame() %>%
    slice(nrow(.)) %>%
    gather(actual, error_rate, seq_len(ncol(.))) %>%
    filter(!(actual %in% c("OOB", "Test")))
}

dual_error_data <- bind_rows(
  definite = bind_rows(
    motif = error(artist_motif_forest),
    composition = error(artist_comp_forest),
    .id = "predictor_type"),
  unknown = bind_rows(
    motif = error(artist_motif_forest, test = TRUE),
    composition = error(artist_comp_forest, test = TRUE),
    .id = "predictor_type"),
  .id = "is_definite") %>%
  spread(predictor_type, value = error_rate) %>%
  unite("vals", motif, composition) %>%
  spread(is_definite, value = vals) %>%
  separate(definite, into = c("definite_motif", "definite_composition"), sep = "_", convert = TRUE) %>%
  separate(unknown, into = c("unknown_motif", "unknown_composition"), sep = "_", convert = TRUE)

dual_error_plot <- function(df, pop = NULL) {

  df <- df %>%
    inner_join(dt_artist_attributes, by = c("actual" = "artist")) %>%
    mutate(artist_label = paste0(actual, " (", birth_early_year, "-", death_late_year, ")"))

  if (is.null(pop)) {
    p <- ggplot(df, aes(x = definite_composition, y = definite_motif, color = actual))
  } else {
    p <- ggplot(df, aes(x = definite_composition, y = definite_motif, color = actual, alpha = actual %in% pop)) +
      scale_alpha_discrete(range = c(0.4, 1), guide = FALSE)
  }

  p +
    geom_hline(yintercept = 0.5, linetype = 2, color = "gray") +
    geom_vline(xintercept = 0.5, linetype = 2, color = "gray") +
    geom_point(size = 3) +
    geom_label_repel(aes(label = artist_label)) +
    scale_color_brewer(palette = "Dark2", guide = FALSE) +
    theme_bw() +
    xlim(0, 1) +
    ylim(0, 1)
}

dual_error_plot(dual_error_data)

ggsave(dual_error_plot(dual_error_data), filename = "~/Desktop/all_artists.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = c("Gerrit Heda", "Willem Claesz Heda")), filename = "~/Desktop/the_hedas.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = "Cornelis Mahu"), filename = "~/Desktop/manu.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = "Floris van Dijck"), filename = "~/Desktop/fvd.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = "Floris van Schooten"), filename = "~/Desktop/fvs.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = "Pieter Claesz"), filename = "~/Desktop/pc.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = c("Pieter Claesz", "Cornelis Mahu")), filename = "~/Desktop/pc_et_al.png", width = 8, height = 7)
ggsave(dual_error_plot(dual_error_data, pop = c("Pieter Claesz", "Gerrit Heda", "Willem Claesz Heda")), filename = "~/Desktop/pc_and_heda.png", width = 8, height = 7)

# Feature power ----

globally_common_vars <- dt_valid %>% select(one_of(motif_vars())) %>%
  gather(motif_code, is_present, everything()) %>%
  filter(is_present == "TRUE") %>%
  count(motif_code, sort = TRUE) %>%
  inner_join(dt_motif_labels, by = "motif_code")

mk_dummy <- function(df, varname) {
  df %>%
    mutate(is_present = 1L) %>%
    spread_(key_col = varname, value_col = "is_present", fill = 0L, sep = "_")
}

tl_motifs <- dt_motif_taxonomy %>%
  filter(is_top_level) %>%
  .$motif_code

motif_dist <- dt_valid %>%
  select(one_of(tl_motifs)) %>%
  mutate_all(funs(if_else(. == TRUE, 1L, 0L))) %>%
  data.matrix() %>%
  t() %>%
  dist(method = "manhattan")

cmdscale(motif_dist) %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  left_join(select(dt_motif_labels, motif_code, wrapped_motif_label), by = c("rowname" = "motif_code")) %>%
  ggplot(aes(x = V1, y = V2)) +
  geom_point() +
  geom_label_repel(aes(label = wrapped_motif_label))

motif_dist_df <- motif_dist %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "origin") %>%
  gather(target, distance, -origin) %>%
  mutate(dist_rank = ntile(distance, 100)) %>%
  arrange(desc(distance)) %>%
  left_join(select(dt_motif_labels, motif_code, origin_label = wrapped_motif_label), by = c("origin" = "motif_code")) %>%
  left_join(select(dt_motif_labels, motif_code, target_label = wrapped_motif_label), by = c("target" = "motif_code")) %>%
  mutate(
    origin = if_else(is.na(origin_label), origin, origin_label),
    target = if_else(is.na(target_label), target, target_label)) %>%
  select(origin, target, distance, dist_rank)


motif_dist_df %>%
  filter(origin == "Shellfish") %>%
  mutate(newtile = ntile(distance, 100)) %>%
  filter(target %in% c("Mince pie", "Bread", "Overturned Tazza", "Broken glass")) %>%
  ggplot(aes(x = target, y = distance)) +
  geom_bar(stat = "identity") +
  geom_hline(aes(yintercept = median(distance))) +
  coord_flip()


fvd_pca <- prcomp(fvd)$rotation %>%
  as.data.frame() %>%
  select(1:2) %>%
  rownames_to_column(var = "motif_code") %>%
  left_join(dt_motif_labels, by = "motif_code") %>%
  mutate(power = sqrt(PC1 ^ 2 + PC2 ^ 2),
         type = if_else(is.na(wrapped_motif_label), "compositional", "motif"),
         text = if_else(is.na(wrapped_motif_label), motif_code, wrapped_motif_label)) %>%
  arrange(desc(power)) %>%
  slice(1:10)

ggplot(fvd_pca, aes(xend = PC1, yend = PC2, color = type)) +
  geom_segment(aes(x = 0, y = 0)) +
  geom_label_repel(aes(label = text, x = PC1, y = PC2))

# Tween plot ----

dual_error_data <- bind_rows(
  definite = bind_rows(
    motif = error(artist_motif_forest),
    composition = error(artist_comp_forest),
    .id = "predictor_type"),
  unknown = bind_rows(
    motif = error(artist_motif_forest, test = TRUE),
    composition = error(artist_comp_forest, test = TRUE),
    .id = "predictor_type"),
  .id = "is_definite") %>%
  spread(predictor_type, error_rate)

kdata <- dual_error_data %>%
  mutate_if(is.character, funs(as.factor)) %>%
  split(f = dual_error_data$is_definite)


ts <- tween_states(kdata, tweenlength = 10, statelength = 2, ease = "quadratic-in-out", nframes = 5 * 32)

tplot <- function(ts, pop = NULL) {

  if (is.null(pop)) {
    p <- ggplot(ts, aes(x = composition, y = motif, color = actual, frame = .frame))
  } else {
    p <- ggplot(ts, aes(x = composition, y = motif, color = actual, frame = .frame, alpha = actual == pop)) +
      scale_alpha_discrete(guide = FALSE)
  }

  p <- p +
    geom_hline(yintercept = 0.5, linetype = 2, color = "gray") +
    geom_vline(xintercept = 0.5, linetype = 2, color = "gray") +
    geom_point(size = 7) +
    geom_label_repel(aes(label = actual), size = 5) +
    scale_color_brewer(palette = "Dark2", guide = FALSE) +
    theme_bw(base_size = 16) +
    xlim(0, 1) +
    ylim(0, 1)

  p
}


gg_animate_save(gg_animate(tplot(ts), title_frame = FALSE), filename = "~/Desktop/all_points.mp4", interval = 1/32, ani.width = 768, ani.height = 768)

walk(cleared_artists, function(x) gg_animate_save(gg_animate(tplot(ts, pop = x), title_frame = FALSE), filename = paste0("~/Desktop/", tolower(str_replace_all(x, " ", "_")), ".mp4"), interval = 1/32, ani.width = 768, ani.height = 768))


# confusion ----


confusion <- function(rf) {
  rf[["test"]][["confusion"]] %>%
    as.data.frame() %>%
    rownames_to_column(var = "actual") %>%
    gather(predicted, times, 2:ncol(.)) %>%
    filter(predicted != "class.error")
}

write_clip(data.frame(actual = artist_union_data$unky, motif_predicted = artist_motif_forest$test$predicted, composiiton_predicted = artist_comp_forest$test$predicted))


