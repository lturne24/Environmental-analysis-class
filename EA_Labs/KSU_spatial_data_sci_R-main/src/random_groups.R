# group assignments


# Example list of student names
student_names <- c("Daiyanera", "David", "Kaylie", "Tanjina", "Cadence", "Srishti", 
                   "Pam", "Aciano", "Andrew", "Max", "Lydia")

# Set group size
group_size <- 3

# Shuffle student names randomly
set.seed(123)  # For reproducibility
shuffled_students <- sample(student_names)

# Calculate number of groups
num_groups <- ceiling(length(shuffled_students) / group_size)

# Assign students to groups
group_assignments <- split(shuffled_students, rep(1:num_groups, each = group_size, length.out = length(shuffled_students)))

# Display the groups
for (i in seq_along(group_assignments)) {
  cat(paste("Group", i, ":", paste(group_assignments[[i]], collapse = ", ")), "\n")
}
