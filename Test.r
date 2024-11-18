# Đọc dữ liệu
GPU_data <- read.csv("All_GPUs.csv")

# Chọn các biến cần rồi lưu vào data
library("dplyr")
data <- GPU_data %>% select("Release_Price", "Core_Speed", "Manufacturer", "Memory", "Memory_Bandwidth", "Memory_Speed", "Max_Power")
str(data)

# Loại bỏ ký tự thừa trong các cột
data$Release_Price <- gsub("P\\$", "", data$Release_Price)
data$Core_Speed <- gsub(" MHz", "", data$Core_Speed)
data$Memory <- gsub(" MB", "", data$Memory)
data$Memory_Bandwidth <- gsub("GB/sec", "", data$Memory_Bandwidth)
data$Memory_Speed <- gsub(" MHz", "", data$Memory_Speed)
data$Max_Power <- gsub(" Watts", "", data$Max_Power)

# Chuyển các cột thành dạng numeric"
data$Release_Price <- as.numeric(data$Release_Price)
data$Core_Speed <- as.numeric(data$Core_Speed)
data$Memory <- as.numeric(data$Memory)
data$Memory_Bandwidth <- as.numeric(data$Memory_Bandwidth)
data$Memory_Speed <- as.numeric(data$Memory_Speed)
data$Max_Power <- as.numeric(data$Max_Power)

# Nhập dữ liệu cột Best_Resolution để tính number_of_pixels
GPU_data$Best_Resolution <- as.character(GPU_data$Best_Resolution)

# Tách cột Best_Resolution thành hai phần: width và heights
GPU_data$res_split <- strsplit(GPU_data$Best_Resolution, " x ")
GPU_data$width <- sapply(GPU_data$res_split, function(x) as.numeric(x[1]))
GPU_data$height <- sapply(GPU_data$res_split, function(x) as.numeric(x[2]))
# Tính number_of_pixels
data$number_of_pixels <- GPU_data$width * GPU_data$height

# Chuyển Manufacturer thành dạng factor
data$Manufacturer <- as.factor(data$Manufacturer)

cat("\n\n-- Kiểm tra số lượng và tỷ lệ dữ liệu khuyết --")
library(inspectdf)
print(inspect_na(data))

# Thay thế NA bằng giá trị trung bình của từng cột
numeric_columns <- sapply(data, is.numeric)
data[numeric_columns] <- lapply(data[numeric_columns], function(col) {
  col[is.na(col)] <- mean(col, na.rm = TRUE)
  return(col)
})

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(Release_Price ~ ., data)

# Tóm tắt kết quả mô hình
cat("\n\n-- Tóm tắt kết quả mô hình --")
print(summary(model))
