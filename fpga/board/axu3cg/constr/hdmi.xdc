
############# clock define##################
#set_property PACKAGE_PIN AE5 [get_ports sys_clk]
##set_property IOSTANDARD LVCMOS18 [get_ports sys_clk]
#set_property IOSTANDARD LVDS [get_ports sys_clk]
##set_property PACKAGE_PIN AF5 [get_ports sys_clk_n]
##set_property IOSTANDARD LVCMOS18 [get_ports sys_clk_n]
########################fan setting#########################
#set_property IOSTANDARD LVCMOS15 [get_ports fan]
#set_property PACKAGE_PIN B5 [get_ports fan]

#################9134 setting##############################

## FPGA pin to hdmi signal
#set_property PACKAGE_PIN A1 [get_ports hdmi_clk]
#set_property PACKAGE_PIN C8 [get_ports {hdmi_rgb[0]}]
#set_property PACKAGE_PIN A3 [get_ports {hdmi_rgb[1]}]
#set_property PACKAGE_PIN B1 [get_ports {hdmi_rgb[2]}]
#set_property PACKAGE_PIN B3 [get_ports {hdmi_rgb[3]}]
#set_property PACKAGE_PIN C1 [get_ports {hdmi_rgb[4]}]
#set_property PACKAGE_PIN B6 [get_ports {hdmi_rgb[5]}]
#set_property PACKAGE_PIN A2 [get_ports {hdmi_rgb[6]}]
#set_property PACKAGE_PIN C6 [get_ports {hdmi_rgb[7]}]
#set_property PACKAGE_PIN F3 [get_ports {hdmi_rgb[8]}]
#set_property PACKAGE_PIN D6 [get_ports {hdmi_rgb[9]}]
#set_property PACKAGE_PIN D1 [get_ports {hdmi_rgb[10]}]
#set_property PACKAGE_PIN D7 [get_ports {hdmi_rgb[11]}]
#set_property PACKAGE_PIN E1 [get_ports {hdmi_rgb[12]}]
#set_property PACKAGE_PIN F5 [get_ports {hdmi_rgb[13]}]
#set_property PACKAGE_PIN F6 [get_ports {hdmi_rgb[14]}]
#set_property PACKAGE_PIN F7 [get_ports {hdmi_rgb[15]}]
#set_property PACKAGE_PIN G6 [get_ports {hdmi_rgb[16]}]
#set_property PACKAGE_PIN G5 [get_ports {hdmi_rgb[17]}]
#set_property PACKAGE_PIN G8 [get_ports {hdmi_rgb[18]}]
#set_property PACKAGE_PIN F1 [get_ports {hdmi_rgb[19]}]
#set_property PACKAGE_PIN E2 [get_ports {hdmi_rgb[20]}]
#set_property PACKAGE_PIN G1 [get_ports {hdmi_rgb[21]}]
#set_property PACKAGE_PIN F2 [get_ports {hdmi_rgb[22]}]
#set_property PACKAGE_PIN D4 [get_ports {hdmi_rgb[23]}]
#set_property PACKAGE_PIN B4 [get_ports hdmi_videovalid]
#set_property PACKAGE_PIN B8 [get_ports hdmi_hsync]
#set_property PACKAGE_PIN G3 [get_ports hdmi_nreset]
#set_property PACKAGE_PIN A4 [get_ports hdmi_vsync]
#set_property PACKAGE_PIN E4 [get_ports hdmi_scl]
#set_property PACKAGE_PIN E3 [get_ports hdmi_sda]
#
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_clk]
#set_property IOSTANDARD LVCMOS15 [get_ports {hdmi_rgb[*]}]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_videovalid]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_hsync]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_nreset]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_vsync]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_scl]
#set_property IOSTANDARD LVCMOS15 [get_ports hdmi_sda]
#
#set_property SLEW FAST [get_ports {hdmi_rgb[*]}]
#set_property SLEW FAST [get_ports hdmi_videovalid]
#set_property SLEW FAST [get_ports hdmi_hsync]
#set_property SLEW FAST [get_ports hdmi_vsync]
