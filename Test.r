# Đọc dữ liệu
data <- read.csv("ALL_GPUs.csv")

# Xem tổng quan dữ liệu
str(data)
summary(data)

# Kiểm tra giá trị thiếu
sum(is.na(data))

# Loại bỏ các hàng có giá trị thiếu
data <- na.omit(data)

# Hoặc thay thế giá trị thiếu bằng trung bình (nếu cần)
# data$number_of_pixels[is.na(data$number_of_pixels)] <- mean(data$number_of_pixels, na.rm = TRUE)

# Chuyển Manufacturer thành dạng factor
data$Manufacturer <- as.factor(data$Manufacturer)

# Giả sử cột Best_resolution là dạng chuỗi
data$Best_resolution <- as.character(data$Best_resolution)

# Tách cột độ phân giải thành hai phần: width và height
data$res_split <- strsplit(data$Best_resolution, "x")
data$width <- sapply(data$res_split, function(x) as.numeric(x[1]))
data$height <- sapply(data$res_split, function(x) as.numeric(x[2]))

# Tính tổng số pixel
data$number_of_pixels <- data$width * data$height

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(Release_Price ~ number_of_pixels + Core_Speed + Memory + 
            Memory_Bandwidth + Memory_Speed + Manufacturer + Max_Power, 
            data = data)

# Tóm tắt kết quả mô hình
summary(model)

# Phân tích phần dư
residuals <- residuals(model)

# Biểu đồ histogram của phần dư
hist(residuals, main = "Residuals Histogram", xlab = "Residuals")

# Kiểm tra phân phối chuẩn (Shapiro-Wilk test)
shapiro.test(residuals)

# Cài đặt gói car (nếu chưa cài)
if(!require(car)) install.packages("car")

# Kiểm tra VIF
library(car)
vif(model)

# Kiểm tra Durbin-Watson
if(!require(lmtest)) install.packages("lmtest")
library(lmtest)

dwtest(model)

# Dự đoán với dữ liệu mới
new_data <- data.frame(
  number_of_pixels = c(1000, 1200),
  core_speed_value = c(1.5, 1.7),
  memory_value = c(8, 16),
  memory_bandwidth_value = c(256, 320),
  memory_speed_value = c(14, 16),
  manufacturer = factor(c("ManufacturerA", "ManufacturerB"), levels = levels(data$manufacturer)),
  max_power_value = c(120, 150)
)

predictions <- predict(model, newdata = new_data)
print(predictions)