files = [ "dsp_cores_pkg.vhd" ];

modules = { "local" : [	"wb_un_cross",
			"multiplier",
			"cic",
			"cordic",
			"divider",
			"fixed_dds",
			"mixer",
			"downconv",
			"delta_sigma",
			"position_nosysgen"
                      ] };
