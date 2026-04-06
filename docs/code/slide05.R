tw_data |> filter(month %in% 7:9) |> # 7月-9月を抽出
  ggplot(aes(x = solar, y = temp)) + # x軸を日射量，y軸を気温に設定
  geom_point(colour = "blue", shape = 19) + # 色と形を指定(点の形は '?points' を参照)
  labs(x = "solar radiation", y = "temperature") # 軸の名前を指定

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + # 湿度を点の大きさで表示
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature")

tw_data |> filter(month %in% 7:9) |> 
  ggplot(aes(x = solar, y = temp, size = humid)) + 
  geom_point(colour = "blue", shape = 19) + 
  labs(x = "solar radiation", y = "temperature") +
  scale_x_log10() + scale_y_log10() # x軸，y軸を対数表示
