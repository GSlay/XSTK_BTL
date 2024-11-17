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
data$number_of_pixels[is.na(data$number_of_pixels)] <- mean(data$number_of_pixels, na.rm = TRUE)

# Chuyển manufacturer thành dạng factor
data$manufacturer <- as.factor(data$manufacturer)

# Xây dựng mô hình hồi quy tuyến tính
model <- lm(release_price ~ number_of_pixels + core_speed_value + memory_value + 
            memory_bandwidth_value + memory_speed_value + manufacturer + max_power_value, 
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