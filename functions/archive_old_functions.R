
#' Complete Depth Sequence
#'
#' @param .depth_vec a vector of depth values.
#' @param .by a single numeric value representing the increment of a sequence.
#'
#' @return a numeric vector representing a complete depth sequence.

depth_seq <- function(.depth_vec, .by = 0.1) {
  # Find the max depth to inform the seq.
  max_scalar <- max(.depth_vec,
                    na.rm = TRUE)
  # Create a sequence from 0 to the max determined above.
  seq(from = 0,
      to = max_scalar,
      by = .by)
}


#' Create a Complete Depth Profile
#'
#' @param .profile_df a data frame containing profile data.
#' @param .discrete_df a data frame containing discrete data.
#' @param .depth_col the name of the Depth column in .profile_df and .discrete_df.
#' @param .quite a logical vector indicating if messages should be printed (TRUE)
#' or not (FALSE).
#' @inheritParams depth_seq
#'
#' @return a data frame with a complete depth profile.

depth_complete <- function(.profile_df, .discrete_df,
                           .depth_col = "Depth", .by = 0.1,
                           .quite) {
  # Create a DF where each row represents a depth at a specified interval.
  depth_df <- data.frame(
    # .profile_df is used because it most likely contains the deepest depth
    # observed between the two DFs.
    depth_seq(.depth_vec = .profile_df[.depth_col],
              .by = .by)
  )
  # Rename the depth column based on the supplied .depth_col.
  names(depth_df) <- .depth_col
  # Identify which column names are shared between the two DFs.
  # Will be used to merge by.
  shared_names <- names(.profile_df)[names(.profile_df) %in% names(.discrete_df)]

  data.frame(unique(.profile_df[shared_names]))
  # Join the profile DF with the new Depth DF to create a complete
  # depth profile.
  pro_depth_df <- merge(x = .profile_df,
                        y = depth_df,
                        by = .depth_col,
                        all = TRUE)

  # Indicate which column names will be used to merge the DFs.
  if (.quite != TRUE) message("Merging by: ", paste(shared_names, collapse = ", "))
  # Join the discrete DF with the complete depth profile DF
  # using the shared names vector.
  final_df <- merge(x = .discrete_df,
                    y = pro_depth_df,
                    by = shared_names,
                    all = TRUE)
  # Arrange the DF rows based on the depth.
  final_df <- final_df[order(final_df$Depth), ]
  # Return a DF.
  return(final_df)
}
