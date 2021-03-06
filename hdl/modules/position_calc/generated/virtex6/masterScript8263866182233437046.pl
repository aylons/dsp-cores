
open(PIDFILE, '> pidfile.txt') || die 'Couldn\'t write process ID to file.';
print PIDFILE "$$\n";
close(PIDFILE);

eval {
  # Call script(s).
  my $instrs;
  my $results = [];
$ENV{'SYSGEN'} = 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen';
  use Sg;
  $instrs = {
    'HDLCodeGenStatus' => 0.0,
    'HDL_PATH' => 'D:/Sysgen_projects/dsp-cores-devel/models',
    'Impl_file' => 'ISE Defaults',
    'Impl_file_sgadvanced' => '',
    'Synth_file' => 'XST Defaults',
    'Synth_file_sgadvanced' => '',
    'TEMP' => 'C:/TEMP',
    'TMP' => 'C:/TEMP',
    'Temp' => 'C:/TEMP',
    'Tmp' => 'C:/TEMP',
    'base_system_period_hardware' => 4.441160919464348,
    'base_system_period_simulink' => 4.441160919464348E-9,
    'block_icon_display' => 'Default',
    'block_type' => 'sysgen',
    'block_version' => '',
    'ce_clr' => 1.0,
    'clock_domain' => 'default',
    'clock_loc' => '',
    'clock_wrapper' => 'Clock Enables',
    'clock_wrapper_sgadvanced' => '',
    'compilation' => 'HDL Netlist',
    'compilation_lut' => {
      'keys' => [
        'HDL Netlist',
        '<New compilation target>',
        'ML605 (JTAG)',
        'Bitstream',
        'Timing and Power Analysis',
      ],
      'values' => [
        'target1',
        'target2',
        'target3',
        'target4',
        'target5',
      ],
    },
    'compilation_target' => 'HDL Netlist',
    'core_generation' => 1.0,
    'core_generation_sgadvanced' => '',
    'core_is_deployed' => 0.0,
    'coregen_core_generation_tmpdir' => 'C:/TEMP/sysgentmp-lucas.russo/cg_wk/c535c8db2f51ff4f0',
    'coregen_part_family' => 'virtex6',
    'createTestbench' => 0,
    'create_interface_document' => 'off',
    'dbl_ovrd' => -1.0,
    'dbl_ovrd_sgadvanced' => '',
    'dcm_input_clock_period' => 10.0,
    'deprecated_control' => 'off',
    'deprecated_control_sgadvanced' => '',
    'design' => 'ddc_bpm_476_066',
    'design_full_path' => 'D:\\Sysgen_projects\\dsp-cores-devel\\models\\ddc_bpm_476_066.mdl',
    'device' => 'xc6vlx240t-1ff1156',
    'device_speed' => '-1',
    'directory' => 'D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066',
    'dsp_cache_root_path' => 'C:/TEMP/sysgentmp-lucas.russo',
    'eval_field' => '0',
    'fileDeliveryDefaults' => [
      [
        '(?i)\\.vhd$',
        { 'fileName' => 'D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/perl_results.vhd', },
      ],
      [
        '(?i)\\.v$',
        { 'fileName' => 'D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/perl_results.v', },
      ],
    ],
    'fxdptinstalled' => 1.0,
    'generateUsing71FrontEnd' => 1,
    'generating_island_subsystem_handle' => 2083.000732421875,
    'generating_subsystem_handle' => 2083.000732421875,
    'generation_directory' => './netlist_ddc_bpm_476_066',
    'has_advanced_control' => '0',
    'hdlDir' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl',
    'hdlKind' => 'vhdl',
    'hdl_path' => 'D:/Sysgen_projects/dsp-cores-devel/models',
    'impl_file' => 'ISE Defaults*',
    'incr_netlist' => 'off',
    'incr_netlist_sgadvanced' => '',
    'infoedit' => ' System Generator',
    'isdeployed' => 0,
    'ise_version' => '13.4i',
    'master_sysgen_token_handle' => 2084.0006103515625,
    'matlab' => 'C:/Program Files/MATLAB/R2011b',
    'matlab_fixedpoint' => 1.0,
    'mdlHandle' => 2083.000732421875,
    'mdlPath' => 'D:/Sysgen_projects/dsp-cores-devel/models/ddc_bpm_476_066.mdl',
    'modelDiagnostics' => [
      {
        'count' => 3187.0,
        'isMask' => 0.0,
        'type' => 'ddc_bpm_476_066 Total blocks',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'Clock',
      },
      {
        'count' => 32.0,
        'isMask' => 0.0,
        'type' => 'Constant',
      },
      {
        'count' => 4.0,
        'isMask' => 0.0,
        'type' => 'DataTypeConversion',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'DiscretePulseGenerator',
      },
      {
        'count' => 125.0,
        'isMask' => 0.0,
        'type' => 'Display',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'Fcn',
      },
      {
        'count' => 143.0,
        'isMask' => 0.0,
        'type' => 'From',
      },
      {
        'count' => 8.0,
        'isMask' => 0.0,
        'type' => 'Gain',
      },
      {
        'count' => 134.0,
        'isMask' => 0.0,
        'type' => 'Goto',
      },
      {
        'count' => 519.0,
        'isMask' => 0.0,
        'type' => 'Inport',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'Lookup',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'Math',
      },
      {
        'count' => 418.0,
        'isMask' => 0.0,
        'type' => 'Outport',
      },
      {
        'count' => 5.0,
        'isMask' => 0.0,
        'type' => 'Product',
      },
      {
        'count' => 4.0,
        'isMask' => 0.0,
        'type' => 'Quantizer',
      },
      {
        'count' => 1297.0,
        'isMask' => 0.0,
        'type' => 'S-Function',
      },
      {
        'count' => 4.0,
        'isMask' => 0.0,
        'type' => 'Saturate',
      },
      {
        'count' => 36.0,
        'isMask' => 0.0,
        'type' => 'Scope',
      },
      {
        'count' => 2.0,
        'isMask' => 0.0,
        'type' => 'SignalConversion',
      },
      {
        'count' => 1.0,
        'isMask' => 0.0,
        'type' => 'Sin',
      },
      {
        'count' => 297.0,
        'isMask' => 0.0,
        'type' => 'SubSystem',
      },
      {
        'count' => 6.0,
        'isMask' => 0.0,
        'type' => 'Sum',
      },
      {
        'count' => 5.0,
        'isMask' => 0.0,
        'type' => 'Switch',
      },
      {
        'count' => 127.0,
        'isMask' => 0.0,
        'type' => 'Terminator',
      },
      {
        'count' => 5.0,
        'isMask' => 0.0,
        'type' => 'ToWorkspace',
      },
      {
        'count' => 1.0,
        'isMask' => 0.0,
        'type' => 'UniformRandomNumber',
      },
      {
        'count' => 4.0,
        'isMask' => 0.0,
        'type' => 'ZeroOrderHold',
      },
      {
        'count' => 5.0,
        'isMask' => 1.0,
        'type' => 'Manual Switch',
      },
      {
        'count' => 2.0,
        'isMask' => 1.0,
        'type' => 'Repeating table',
      },
      {
        'count' => 30.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Adder/Subtracter Block',
      },
      {
        'count' => 66.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Arithmetic Relational Operator Block',
      },
      {
        'count' => 27.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Assert Block',
      },
      {
        'count' => 20.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Bit Slice Extractor Block',
      },
      {
        'count' => 3.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Bitwise Expression Evaluator Block',
      },
      {
        'count' => 9.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Bus Concatenator Block',
      },
      {
        'count' => 8.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Bus Multiplexer Block',
      },
      {
        'count' => 5.0,
        'isMask' => 1.0,
        'type' => 'Xilinx CIC Compiler 3.0 Block',
      },
      {
        'count' => 4.0,
        'isMask' => 1.0,
        'type' => 'Xilinx CORDIC 5.0 Block',
      },
      {
        'count' => 8.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Clock Enable Probe Block',
      },
      {
        'count' => 2.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Complex Multiplier 5.0  Block',
      },
      {
        'count' => 79.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Constant Block Block',
      },
      {
        'count' => 8.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Counter Block',
      },
      {
        'count' => 4.0,
        'isMask' => 1.0,
        'type' => 'Xilinx DDS Compiler 5.0 Block',
      },
      {
        'count' => 56.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Delay Block',
      },
      {
        'count' => 9.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Divider Generator 4.0 Block',
      },
      {
        'count' => 79.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Down Sampler Block',
      },
      {
        'count' => 6.0,
        'isMask' => 1.0,
        'type' => 'Xilinx FIR Compiler 6.3 Block',
      },
      {
        'count' => 22.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Gateway In Block',
      },
      {
        'count' => 258.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Gateway Out Block',
      },
      {
        'count' => 24.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Inverter Block',
      },
      {
        'count' => 24.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Logical Block Block',
      },
      {
        'count' => 16.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Multiplier Block',
      },
      {
        'count' => 249.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Register Block',
      },
      {
        'count' => 125.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Sample Time Block Block',
      },
      {
        'count' => 12.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Shared Memory Based From FIFO Block',
      },
      {
        'count' => 12.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Shared Memory Based To FIFO Block',
      },
      {
        'count' => 1.0,
        'isMask' => 1.0,
        'type' => 'Xilinx System Generator Block',
      },
      {
        'count' => 23.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Type Converter Block',
      },
      {
        'count' => 81.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Type Reinterpreter Block',
      },
      {
        'count' => 20.0,
        'isMask' => 1.0,
        'type' => 'Xilinx Up Sampler Block',
      },
      {
        'count' => 1.0,
        'isMask' => 1.0,
        'type' => 'Xilinx WaveScope Block',
      },
    ],
    'model_globals_initialized' => 1.0,
    'model_path' => 'D:/Sysgen_projects/dsp-cores-devel/models/ddc_bpm_476_066.mdl',
    'myxilinx' => 'C:/Xilinx/13.4/ISE_DS/ISE',
    'ngc_files' => [ 'xlpersistentdff.ngc', ],
    'num_sim_cycles' => '450332',
    'package' => 'ff1156',
    'part' => 'xc6vlx240t',
    'partFamily' => 'virtex6',
    'port_data_types_enabled' => 1.0,
    'preserve_hierarchy' => 0.0,
    'proj_type' => 'Project Navigator',
    'proj_type_sgadvanced' => '',
    'provideGlobalCEClr' => 1.0,
    'run_coregen' => 'off',
    'run_coregen_sgadvanced' => '',
    'sample_time_colors_enabled' => 1.0,
    'sampletimecolors' => 1.0,
    'sg_blockgui_xml' => '',
    'sg_icon_stat' => '50,50,-1,-1,token,white,0,07734,right,,[ ],[ ]',
    'sg_list_contents' => '',
    'sg_mask_display' => 'fprintf(\'\',\'COMMENT: begin icon graphics\');
patch([0 50 50 0 0 ],[0 0 50 50 0 ],[1 1 1 ]);
patch([1.6375 16.81 27.31 37.81 48.31 27.31 12.1375 1.6375 ],[36.655 36.655 47.155 36.655 47.155 47.155 47.155 36.655 ],[0.933333 0.203922 0.141176 ]);
patch([12.1375 27.31 16.81 1.6375 12.1375 ],[26.155 26.155 36.655 36.655 26.155 ],[0.698039 0.0313725 0.219608 ]);
patch([1.6375 16.81 27.31 12.1375 1.6375 ],[15.655 15.655 26.155 26.155 15.655 ],[0.933333 0.203922 0.141176 ]);
patch([12.1375 48.31 37.81 27.31 16.81 1.6375 12.1375 ],[5.155 5.155 15.655 5.155 15.655 15.655 5.155 ],[0.698039 0.0313725 0.219608 ]);
fprintf(\'\',\'COMMENT: end icon graphics\');
fprintf(\'\',\'COMMENT: begin icon text\');
fprintf(\'\',\'COMMENT: end icon text\');',
    'sg_version' => '',
    'sggui_pos' => '-1,-1,-1,-1',
    'simulation_island_subsystem_handle' => 2083.000732421875,
    'simulink_accelerator_running' => 0.0,
    'simulink_debugger_running' => 0.0,
    'simulink_period' => 4.441160919464348E-9,
    'speed' => '-1',
    'synth_file' => 'XST Defaults*',
    'synthesisTool' => 'XST',
    'synthesis_language' => 'vhdl',
    'synthesis_tool' => 'XST',
    'synthesis_tool_sgadvanced' => '',
    'sysclk_period' => 4.441160919464348,
    'sysgen' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen',
    'sysgenRoot' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen',
    'sysgenTokenSettings' => {
      'Impl_file' => 'ISE Defaults',
      'Impl_file_sgadvanced' => '',
      'Synth_file' => 'XST Defaults',
      'Synth_file_sgadvanced' => '',
      'base_system_period_hardware' => 4.441160919464348,
      'base_system_period_simulink' => 4.441160919464348E-9,
      'block_icon_display' => 'Default',
      'block_type' => 'sysgen',
      'block_version' => '',
      'ce_clr' => 1.0,
      'clock_loc' => '',
      'clock_wrapper' => 'Clock Enables',
      'clock_wrapper_sgadvanced' => '',
      'compilation' => 'HDL Netlist',
      'compilation_lut' => {
        'keys' => [
          'HDL Netlist',
          '<New compilation target>',
          'ML605 (JTAG)',
          'Bitstream',
          'Timing and Power Analysis',
        ],
        'values' => [
          'target1',
          'target2',
          'target3',
          'target4',
          'target5',
        ],
      },
      'core_generation' => 1.0,
      'core_generation_sgadvanced' => '',
      'coregen_part_family' => 'virtex6',
      'create_interface_document' => 'off',
      'dbl_ovrd' => -1.0,
      'dbl_ovrd_sgadvanced' => '',
      'dcm_input_clock_period' => 10.0,
      'deprecated_control' => 'off',
      'deprecated_control_sgadvanced' => '',
      'directory' => './netlist_ddc_bpm_476_066',
      'eval_field' => '0',
      'has_advanced_control' => '0',
      'impl_file' => 'ISE Defaults*',
      'incr_netlist' => 'off',
      'incr_netlist_sgadvanced' => '',
      'infoedit' => ' System Generator',
      'master_sysgen_token_handle' => 2084.0006103515625,
      'package' => 'ff1156',
      'part' => 'xc6vlx240t',
      'preserve_hierarchy' => 0.0,
      'proj_type' => 'Project Navigator',
      'proj_type_sgadvanced' => '',
      'run_coregen' => 'off',
      'run_coregen_sgadvanced' => '',
      'sg_blockgui_xml' => '',
      'sg_icon_stat' => '50,50,-1,-1,token,white,0,07734,right,,[ ],[ ]',
      'sg_list_contents' => '',
      'sg_mask_display' => 'fprintf(\'\',\'COMMENT: begin icon graphics\');
patch([0 50 50 0 0 ],[0 0 50 50 0 ],[1 1 1 ]);
patch([1.6375 16.81 27.31 37.81 48.31 27.31 12.1375 1.6375 ],[36.655 36.655 47.155 36.655 47.155 47.155 47.155 36.655 ],[0.933333 0.203922 0.141176 ]);
patch([12.1375 27.31 16.81 1.6375 12.1375 ],[26.155 26.155 36.655 36.655 26.155 ],[0.698039 0.0313725 0.219608 ]);
patch([1.6375 16.81 27.31 12.1375 1.6375 ],[15.655 15.655 26.155 26.155 15.655 ],[0.933333 0.203922 0.141176 ]);
patch([12.1375 48.31 37.81 27.31 16.81 1.6375 12.1375 ],[5.155 5.155 15.655 5.155 15.655 15.655 5.155 ],[0.698039 0.0313725 0.219608 ]);
fprintf(\'\',\'COMMENT: end icon graphics\');
fprintf(\'\',\'COMMENT: begin icon text\');
fprintf(\'\',\'COMMENT: end icon text\');',
      'sggui_pos' => '-1,-1,-1,-1',
      'simulation_island_subsystem_handle' => 2083.000732421875,
      'simulink_period' => 4.441160919464348E-9,
      'speed' => '-1',
      'synth_file' => 'XST Defaults*',
      'synthesis_language' => 'vhdl',
      'synthesis_tool' => 'XST',
      'synthesis_tool_sgadvanced' => '',
      'sysclk_period' => 4.441160919464348,
      'testbench' => 0,
      'testbench_sgadvanced' => '',
      'trim_vbits' => 1.0,
      'trim_vbits_sgadvanced' => '',
      'xilinx_device' => 'xc6vlx240t-1ff1156',
      'xilinxfamily' => 'virtex6',
    },
    'sysgen_Root' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen',
    'systemClockPeriod' => 4.441160919464348,
    'tempdir' => 'C:/TEMP',
    'testbench' => 0,
    'testbench_sgadvanced' => '',
    'tmpDir' => 'D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen',
    'trim_vbits' => 1.0,
    'trim_vbits_sgadvanced' => '',
    'use_ce_syn_keep' => 1,
    'use_strict_names' => 1,
    'user_tips_enabled' => 0.0,
    'usertemp' => 'C:/TEMP/sysgentmp-lucas.russo',
    'using71Netlister' => 1,
    'verilog_files' => [
      'conv_pkg.v',
      'synth_reg.v',
      'synth_reg_w_init.v',
      'convert_type.v',
    ],
    'version' => '',
    'vhdl_files' => [
      'conv_pkg.vhd',
      'synth_reg.vhd',
      'synth_reg_w_init.vhd',
    ],
    'vsimtime' => '2200275.000000 ns',
    'xilinx' => 'C:/Xilinx/13.4/ISE_DS/ISE',
    'xilinx_device' => 'xc6vlx240t-1ff1156',
    'xilinx_family' => 'virtex6',
    'xilinx_package' => 'ff1156',
    'xilinx_part' => 'xc6vlx240t',
    'xilinxdevice' => 'xc6vlx240t-1ff1156',
    'xilinxfamily' => 'virtex6',
    'xilinxpart' => 'xc6vlx240t',
  };
  push(@$results, &Sg::setAttributes($instrs));
  use SgDeliverFile;
  $instrs = {
    'collaborationName' => 'conv_pkg.vhd',
    'sourceFile' => 'hdl/conv_pkg.vhd',
    'templateKeyValues' => {},
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'collaborationName' => 'synth_reg.vhd',
    'sourceFile' => 'hdl/synth_reg.vhd',
    'templateKeyValues' => {},
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'collaborationName' => 'synth_reg_w_init.vhd',
    'sourceFile' => 'hdl/synth_reg_w_init.vhd',
    'templateKeyValues' => {},
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'collaborationName' => 'xlpersistentdff.ngc',
    'sourceFile' => 'hdl/xlpersistentdff.ngc',
    'templateKeyValues' => {},
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'collaborationName' => 'xlclockenablegenerator.vhd',
    'sourceFile' => 'hdl/xlclockenablegenerator.vhd',
    'templateKeyValues' => {},
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  use SgGenerateCores;
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 17',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = -0.00031280517578125,-0.00461578369140625,-0.00075531005859375,0.01094818115234375,0.00547027587890625,-0.01494598388671875,-0.01053619384765625,0.010101318359375,0.00511932373046875,0.00281524658203125,0.02346038818359375,-0.01081085205078125,-0.07684326171875,-0.00476837158203125,0.13726043701171875,0.05204010009765625,-0.17452239990234375,-0.1165008544921875,0.16648101806640625,0.16648101806640625,-0.1165008544921875,-0.17452239990234375,0.05204010009765625,0.13726043701171875,-0.00476837158203125,-0.07684326171875,-0.01081085205078125,0.02346038818359375,0.00281524658203125,0.00511932373046875,0.010101318359375,-0.01053619384765625,-0.01494598388671875,0.00547027587890625,0.01094818115234375,-0.00075531005859375,-0.00461578369140625,-0.00031280517578125',
    'CSET columnconfig = 10',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 15',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 16',
    'CSET decimation_rate = 1',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Single_Rate',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 2',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Not_Required',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 1',
    'CSET number_paths = 4',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 24',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Hardware_Oversampling_Rate',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Not_Required',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 2',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_ef8269b30b0e0deb',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '3e5817bba70ee4e1afd0da8d5d57e52b',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_ef8269b30b0e0deb
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_data_tdata:out std_logic_vector(95 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(63 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(95 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path3_ps_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path2_ps_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path1_ps_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path0_ps_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(63 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_path3_ps_net <= m_axis_data_tdata_net(95 downto 72);
  m_axis_data_tdata_path2_ps_net <= m_axis_data_tdata_net(71 downto 48);
  m_axis_data_tdata_path1_ps_net <= m_axis_data_tdata_net(47 downto 24);
  m_axis_data_tdata_path0_ps_net <= m_axis_data_tdata_net(23 downto 0);
  s_axis_data_tdata_net(63 downto 48) <= s_axis_data_tdata_path3;
  s_axis_data_tdata_net(47 downto 32) <= s_axis_data_tdata_path2;
  s_axis_data_tdata_net(31 downto 16) <= s_axis_data_tdata_path1;
  s_axis_data_tdata_net(15 downto 0) <= s_axis_data_tdata_path0;
  m_axis_data_tdata_path3_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 24,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path3_ps_net,
        ce => ce_2,
        clr => \'0\',
        clk => clk_2, 
        o => m_axis_data_tdata_path3
    );
  m_axis_data_tdata_path2_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 24,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path2_ps_net,
        ce => ce_2,
        clr => \'0\',
        clk => clk_2, 
        o => m_axis_data_tdata_path2
    );
  m_axis_data_tdata_path1_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 24,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path1_ps_net,
        ce => ce_2,
        clr => \'0\',
        clk => clk_2, 
        o => m_axis_data_tdata_path1
    );
  m_axis_data_tdata_path0_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 24,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path0_ps_net,
        ce => ce_2,
        clr => \'0\',
        clk => clk_2, 
        o => m_axis_data_tdata_path0
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_2,
        clr => \'0\',
        clk => clk_2, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_2, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_ef8269b30b0e0deb_instance : fr_cmplr_v6_3_ef8269b30b0e0deb
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tvalid=>ce_logic_2
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_2:in std_logic;
    ce_logic_2:in std_logic;
    clk:in std_logic;
    clk_2:in std_logic;
    clk_logic_2:in std_logic;
    m_axis_data_tdata_path0:out std_logic_vector(23 downto 0);
    m_axis_data_tdata_path1:out std_logic_vector(23 downto 0);
    m_axis_data_tdata_path2:out std_logic_vector(23 downto 0);
    m_axis_data_tdata_path3:out std_logic_vector(23 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata_path0:in std_logic_vector(15 downto 0);
    s_axis_data_tdata_path1:in std_logic_vector(15 downto 0);
    s_axis_data_tdata_path2:in std_logic_vector(15 downto 0);
    s_axis_data_tdata_path3:in std_logic_vector(15 downto 0);
    s_axis_data_tready:out std_logic;
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_5c49e68d4a338363199fcba5a3e22356',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '407c63d2892a17198077dad0595f6c8e',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'bf19524b08a31dce16c130ab8b77a158',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: signed((24 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_signed(input_port);
  output_port <= signed_to_std_logic_vector(input_port_1_40);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((24 - 1) downto 0);
    output_port : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_b62f4240f0',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '61af838f77519ac5b4f71eabcf6b86af',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlceprobe.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b51335fdf4b86da9652af9b87f525979',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal sel_1_20: std_logic_vector((1 - 1) downto 0);
  signal d0_1_24: std_logic_vector((24 - 1) downto 0);
  signal d1_1_27: std_logic_vector((24 - 1) downto 0);
  signal unregy_join_6_1: std_logic_vector((24 - 1) downto 0);
begin
  sel_1_20 <= sel;
  d0_1_24 <= d0;
  d1_1_27 <= d1;
  proc_switch_6_1: process (d0_1_24, d1_1_27, sel_1_20)
  is
  begin
    case sel_1_20 is 
      when "0" =>
        unregy_join_6_1 <= d0_1_24;
      when others =>
        unregy_join_6_1 <= d1_1_27;
    end case;
  end process proc_switch_6_1;
  y <= unregy_join_6_1;
end',
      'crippled_entity' => 'is
  port (
    sel : in std_logic_vector((1 - 1) downto 0);
    d0 : in std_logic_vector((24 - 1) downto 0);
    d1 : in std_logic_vector((24 - 1) downto 0);
    y : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'mux_a2121d82da',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'dcd0587b419d479cffdccf7033047348',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'af24922a26b20e718f854e0bfba6bb45',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal rst_1_40: boolean;
  signal en_1_45: boolean;
  signal count_reg_20_23: unsigned((1 - 1) downto 0) := "0";
  signal count_reg_20_23_rst: std_logic;
  signal count_reg_20_23_en: std_logic;
  signal bool_44_4: boolean;
  signal rst_limit_join_44_1: boolean;
  signal count_reg_join_44_1: unsigned((2 - 1) downto 0);
  signal count_reg_join_44_1_en: std_logic;
  signal count_reg_join_44_1_rst: std_logic;
begin
  rst_1_40 <= ((rst) = "1");
  en_1_45 <= ((en) = "1");
  proc_count_reg_20_23: process (clk)
  is
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (count_reg_20_23_rst = \'1\')) then
        count_reg_20_23 <= "0";
      elsif ((ce = \'1\') and (count_reg_20_23_en = \'1\')) then 
        count_reg_20_23 <= count_reg_20_23 + std_logic_vector_to_unsigned("1");
      end if;
    end if;
  end process proc_count_reg_20_23;
  bool_44_4 <= rst_1_40 or false;
  proc_if_44_1: process (bool_44_4, count_reg_20_23, en_1_45)
  is
  begin
    if bool_44_4 then
      count_reg_join_44_1_rst <= \'1\';
    elsif en_1_45 then
      count_reg_join_44_1_rst <= \'0\';
    else 
      count_reg_join_44_1_rst <= \'0\';
    end if;
    if en_1_45 then
      count_reg_join_44_1_en <= \'1\';
    else 
      count_reg_join_44_1_en <= \'0\';
    end if;
    if bool_44_4 then
      rst_limit_join_44_1 <= false;
    elsif en_1_45 then
      rst_limit_join_44_1 <= false;
    else 
      rst_limit_join_44_1 <= false;
    end if;
  end process proc_if_44_1;
  count_reg_20_23_rst <= count_reg_join_44_1_rst;
  count_reg_20_23_en <= count_reg_join_44_1_en;
  op <= unsigned_to_std_logic_vector(count_reg_20_23);
end',
      'crippled_entity' => 'is
  port (
    rst : in std_logic_vector((1 - 1) downto 0);
    en : in std_logic_vector((1 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'counter_41314d726b',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '40a30dbd7b17e6c1877beeff44d1cd8a',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlusamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '13366d021ddc9f5413827bc05cb9e24f',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "1";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_6293007044',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT DDS_Compiler virtex6 Xilinx,_Inc. 5.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET amplitude_mode = Unit_Circle',
    'CSET channels = 1',
    'CSET data_has_tlast = Not_Required',
    'CSET dds_clock_rate = 112.58317567568',
    'CSET dsp48_use = Minimal',
    'CSET explicit_period = true',
    'CSET frequency_resolution = 0.40000000000',
    'CSET gui_behaviour = Sysgen',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET has_phase_out = false',
    'CSET has_tready = true',
    'CSET latency = 16',
    'CSET latency_configuration = Auto',
    'CSET m_data_has_tuser = Not_Required',
    'CSET m_phase_has_tuser = Not_Required',
    'CSET memory_type = Auto',
    'CSET negative_cosine = false',
    'CSET negative_sine = true',
    'CSET noise_shaping = Taylor_Series_Corrected',
    'CSET optimization_goal = Auto',
    'CSET output_frequency1 = 0',
    'CSET output_frequency10 = 0',
    'CSET output_frequency11 = 0',
    'CSET output_frequency12 = 0',
    'CSET output_frequency13 = 0',
    'CSET output_frequency14 = 0',
    'CSET output_frequency15 = 0',
    'CSET output_frequency16 = 0',
    'CSET output_frequency2 = 0',
    'CSET output_frequency3 = 0',
    'CSET output_frequency4 = 0',
    'CSET output_frequency5 = 0',
    'CSET output_frequency6 = 0',
    'CSET output_frequency7 = 0',
    'CSET output_frequency8 = 0',
    'CSET output_frequency9 = 0',
    'CSET output_selection = Sine_and_Cosine',
    'CSET output_width = 24',
    'CSET parameter_entry = Hardware_Parameters',
    'CSET partspresent = Phase_Generator_and_SIN_COS_LUT',
    'CSET period = 2',
    'CSET phase_increment = Programmable',
    'CSET phase_offset = Programmable',
    'CSET phase_offset_angles1 = 0',
    'CSET phase_offset_angles10 = 0',
    'CSET phase_offset_angles11 = 0',
    'CSET phase_offset_angles12 = 0',
    'CSET phase_offset_angles13 = 0',
    'CSET phase_offset_angles14 = 0',
    'CSET phase_offset_angles15 = 0',
    'CSET phase_offset_angles16 = 0',
    'CSET phase_offset_angles2 = 0',
    'CSET phase_offset_angles3 = 0',
    'CSET phase_offset_angles4 = 0',
    'CSET phase_offset_angles5 = 0',
    'CSET phase_offset_angles6 = 0',
    'CSET phase_offset_angles7 = 0',
    'CSET phase_offset_angles8 = 0',
    'CSET phase_offset_angles9 = 0',
    'CSET phase_width = 30',
    'CSET pinc1 = 1110101000001110101000001110',
    'CSET pinc10 = 0',
    'CSET pinc11 = 0',
    'CSET pinc12 = 0',
    'CSET pinc13 = 0',
    'CSET pinc14 = 0',
    'CSET pinc15 = 0',
    'CSET pinc16 = 0',
    'CSET pinc2 = 0',
    'CSET pinc3 = 0',
    'CSET pinc4 = 0',
    'CSET pinc5 = 0',
    'CSET pinc6 = 0',
    'CSET pinc7 = 0',
    'CSET pinc8 = 0',
    'CSET pinc9 = 0',
    'CSET poff1 = 0',
    'CSET poff10 = 0',
    'CSET poff11 = 0',
    'CSET poff12 = 0',
    'CSET poff13 = 0',
    'CSET poff14 = 0',
    'CSET poff15 = 0',
    'CSET poff16 = 0',
    'CSET poff2 = 0',
    'CSET poff3 = 0',
    'CSET poff4 = 0',
    'CSET poff5 = 0',
    'CSET poff6 = 0',
    'CSET poff7 = 0',
    'CSET poff8 = 0',
    'CSET poff9 = 0',
    'CSET por_mode = false',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_phase_has_tuser = Not_Required',
    'CSET s_phase_tuser_width = 1',
    'CSET spurious_free_dynamic_range = 36',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = dds_cmplr_v5_0_757016b8a434f5d8',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '64735d165ef76f125b3a312a410f9be7',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component dds_cmplr_v5_0_757016b8a434f5d8
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_data_tdata:out std_logic_vector(47 downto 0);
      m_axis_data_tready:in std_logic;
      m_axis_data_tvalid:out std_logic;
      s_axis_config_tdata:in std_logic_vector(63 downto 0);
      s_axis_config_tready:out std_logic;
      s_axis_config_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal s_axis_config_tdata_net: std_logic_vector(63 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_sine <= m_axis_data_tdata_net(47 downto 24);
  m_axis_data_tdata_cosine <= m_axis_data_tdata_net(23 downto 0);
  s_axis_config_tdata_net(61 downto 32) <= s_axis_config_tdata_poff;
  s_axis_config_tdata_net(29 downto 0) <= s_axis_config_tdata_pinc;
  dds_cmplr_v5_0_757016b8a434f5d8_instance : dds_cmplr_v5_0_757016b8a434f5d8
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tready=>m_axis_data_tready,
      m_axis_data_tvalid=>m_axis_data_tvalid,
      s_axis_config_tdata=>s_axis_config_tdata_net,
      s_axis_config_tready=>s_axis_config_tready,
      s_axis_config_tvalid=>s_axis_config_tvalid
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    clk:in std_logic;
    m_axis_data_tdata_cosine:out std_logic_vector(23 downto 0);
    m_axis_data_tdata_sine:out std_logic_vector(23 downto 0);
    m_axis_data_tready:in std_logic;
    m_axis_data_tvalid:out std_logic;
    s_axis_config_tdata_pinc:in std_logic_vector(29 downto 0);
    s_axis_config_tdata_poff:in std_logic_vector(29 downto 0);
    s_axis_config_tready:out std_logic;
    s_axis_config_tvalid:in std_logic
  );
end',
      'entity_name' => 'xldds_compiler_6ef2934d572b7cd1757292caa5710be5',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '26c90b101ce1ca8b2f28c242a8215ef7',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "0";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_963ed6358a',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '97d1de979866be39b04d8dc5fb119bf5',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'd6613d53754b62706c77c9076d87a876',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: unsigned((1 - 1) downto 0);
  signal b_1_34: unsigned((1 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (1 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    0 => false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal result_12_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_unsigned(a);
  b_1_34 <= std_logic_vector_to_unsigned(b);
  op_mem_32_22_back <= op_mem_32_22(0);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  result_12_3_rel <= a_1_31 = b_1_34;
  op_mem_32_22_front_din <= result_12_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((1 - 1) downto 0);
    b : in std_logic_vector((1 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_a892e1bf40',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '345a4c0384276159ac94c12ab3c475e2',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT CORDIC virtex6 Xilinx,_Inc. 5.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET aclken = true',
    'CSET architectural_configuration = Parallel',
    'CSET aresetn = false',
    'CSET cartesian_has_tlast = false',
    'CSET cartesian_has_tuser = true',
    'CSET cartesian_tuser_width = 1',
    'CSET coarse_rotation = true',
    'CSET compensation_scaling = LUT_based',
    'CSET data_format = SignedFraction',
    'CSET flow_control = NonBlocking',
    'CSET functional_selection = Translate',
    'CSET input_width = 25',
    'CSET iterations = 0',
    'CSET optimize_goal = Resources',
    'CSET out_tlast_behv = Null',
    'CSET out_tready = false',
    'CSET output_width = 24',
    'CSET phase_format = Radians',
    'CSET phase_has_tlast = false',
    'CSET phase_has_tuser = false',
    'CSET phase_tuser_width = 1',
    'CSET pipelining_mode = Maximum',
    'CSET precision = 0',
    'CSET round_mode = Nearest_Even',
    'CSET component_name = crdc_v5_0_19fb63dead3076ad',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '1566b24a9af6ffd343c388356590aa21',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component crdc_v5_0_19fb63dead3076ad
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_dout_tdata:out std_logic_vector(47 downto 0);
      m_axis_dout_tuser:out std_logic_vector(0 downto 0);
      m_axis_dout_tvalid:out std_logic;
      s_axis_cartesian_tdata:in std_logic_vector(63 downto 0);
      s_axis_cartesian_tuser:in std_logic_vector(0 downto 0);
      s_axis_cartesian_tvalid:in std_logic
    );
end component;
signal m_axis_dout_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal m_axis_dout_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal s_axis_cartesian_tdata_net: std_logic_vector(63 downto 0) := (others=>\'0\');
signal s_axis_cartesian_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
begin
  m_axis_dout_tdata_phase <= m_axis_dout_tdata_net(47 downto 24);
  m_axis_dout_tdata_real <= m_axis_dout_tdata_net(23 downto 0);
  m_axis_dout_tuser_cartesian_tuser <= m_axis_dout_tuser_net(0 downto 0);
  s_axis_cartesian_tdata_net(56 downto 32) <= s_axis_cartesian_tdata_imag;
  s_axis_cartesian_tdata_net(24 downto 0) <= s_axis_cartesian_tdata_real;
  s_axis_cartesian_tuser_net(0 downto 0) <= s_axis_cartesian_tuser_user;
  crdc_v5_0_19fb63dead3076ad_instance : crdc_v5_0_19fb63dead3076ad
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_dout_tdata=>m_axis_dout_tdata_net,
      m_axis_dout_tuser=>m_axis_dout_tuser_net,
      m_axis_dout_tvalid=>m_axis_dout_tvalid,
      s_axis_cartesian_tdata=>s_axis_cartesian_tdata_net,
      s_axis_cartesian_tuser=>s_axis_cartesian_tuser_net,
      s_axis_cartesian_tvalid=>s_axis_cartesian_tvalid
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    clk:in std_logic;
    m_axis_dout_tdata_phase:out std_logic_vector(23 downto 0);
    m_axis_dout_tdata_real:out std_logic_vector(23 downto 0);
    m_axis_dout_tuser_cartesian_tuser:out std_logic_vector(0 downto 0);
    m_axis_dout_tvalid:out std_logic;
    s_axis_cartesian_tdata_imag:in std_logic_vector(24 downto 0);
    s_axis_cartesian_tdata_real:in std_logic_vector(24 downto 0);
    s_axis_cartesian_tuser_user:in std_logic_vector(0 downto 0);
    s_axis_cartesian_tvalid:in std_logic
  );
end',
      'entity_name' => 'xlcordic_67422259e33cafe86cb2beaf1e4ed91a',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '8bca0a84e26ac56237b68888a8f87c3d',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlconvert.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '6b792f435372e466c617f507f31459f6',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: signed((25 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_signed(input_port);
  output_port <= signed_to_std_logic_vector(input_port_1_40);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((25 - 1) downto 0);
    output_port : out std_logic_vector((25 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_31a4235b32',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '263e5488cedc91b0703679b5442781d4',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '1c15e43b2cb0e2493cbb9c38d1c14235',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "00";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((2 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_cda50df78a',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '4fc55331bd77a545f89c523369ef0364',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ff1f2ebe802f9b6c778c02678a085410',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'bb80b56b3acc7fbb41559cb90cbfc692',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '1000af5570a2adbd6907605458a002a7',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: unsigned((1 - 1) downto 0);
  signal b_1_34: unsigned((2 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (2 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    false,
    false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal cast_12_12: unsigned((2 - 1) downto 0);
  signal result_12_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_unsigned(a);
  b_1_34 <= std_logic_vector_to_unsigned(b);
  op_mem_32_22_back <= op_mem_32_22(1);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        for i in 1 downto 1 loop 
          op_mem_32_22(i) <= op_mem_32_22(i-1);
        end loop;
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  cast_12_12 <= u2u_cast(a_1_31, 0, 2, 0);
  result_12_3_rel <= cast_12_12 = b_1_34;
  op_mem_32_22_front_din <= result_12_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((1 - 1) downto 0);
    b : in std_logic_vector((2 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_ca864fa999',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT CIC_Compiler virtex6 Xilinx,_Inc. 3.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET clock_frequency = 200',
    'CSET differential_delay = 2',
    'CSET filter_type = Decimation',
    'CSET fixed_or_initial_rate = 1112',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 556',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET has_dout_tready = false',
    'CSET input_data_width = 24',
    'CSET input_sample_frequency = 0.00100000000',
    'CSET maximum_rate = 1112',
    'CSET minimum_rate = 1112',
    'CSET number_of_channels = 2',
    'CSET number_of_stages = 5',
    'CSET output_data_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET quantization = Truncation',
    'CSET ratespecification = Maximum_Possible',
    'CSET response_magnitude = Normalized',
    'CSET sample_rate_changes = Fixed',
    'CSET sampleperiod = 1',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET use_streaming_interface = true',
    'CSET use_xtreme_dsp_slice = true',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = cc_cmplr_v3_0_d31b0f3a494d423b',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'd85788bbb9b14a646ffbcccc92d2d9f8',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component cc_cmplr_v3_0_d31b0f3a494d423b
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_tlast_missing:out std_logic;
      event_tlast_unexpected:out std_logic;
      m_axis_data_tdata:out std_logic_vector(31 downto 0);
      m_axis_data_tlast:out std_logic;
      m_axis_data_tuser:out std_logic_vector(15 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(23 downto 0);
      s_axis_data_tlast:in std_logic;
      s_axis_data_tready:out std_logic;
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net_captured: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net_or_captured_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tlast_ps_net: std_logic := \'0\';
signal m_axis_data_tlast_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tlast_ps_net_or_captured_net: std_logic := \'0\';
signal m_axis_data_tuser_net: std_logic_vector(15 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net_captured: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net_or_captured_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net_captured: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net_or_captured_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(23 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_data_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chan_sync_ps_net <= m_axis_data_tuser_net(8 downto 8);
  m_axis_data_tuser_chan_out_ps_net <= m_axis_data_tuser_net(0 downto 0);
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata_data;
  m_axis_data_tdata_data_ps_net_or_captured_net <= m_axis_data_tdata_data_ps_net or m_axis_data_tdata_data_ps_net_captured;
m_axis_data_tdata_data_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_data_ps_net_or_captured_net,
        ce => ce_1112,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tdata_data
    );
m_axis_data_tdata_data_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_data_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tdata_data_ps_net_captured
    );
  m_axis_data_tlast_ps_net_or_captured_net <= m_axis_data_tlast_ps_net or m_axis_data_tlast_ps_net_captured;
m_axis_data_tlast_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tlast_ps_net_or_captured_net,
        ce => ce_1112,
        clr => \'0\',
        clk => clk_1112, 
        o(0) => m_axis_data_tlast
    );
m_axis_data_tlast_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tlast_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1112, 
        o(0) => m_axis_data_tlast_ps_net_captured
    );
  m_axis_data_tuser_chan_sync_ps_net_or_captured_net <= m_axis_data_tuser_chan_sync_ps_net or m_axis_data_tuser_chan_sync_ps_net_captured;
m_axis_data_tuser_chan_sync_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_sync_ps_net_or_captured_net,
        ce => ce_1112,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tuser_chan_sync
    );
m_axis_data_tuser_chan_sync_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_sync_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tuser_chan_sync_ps_net_captured
    );
  m_axis_data_tuser_chan_out_ps_net_or_captured_net <= m_axis_data_tuser_chan_out_ps_net or m_axis_data_tuser_chan_out_ps_net_captured;
m_axis_data_tuser_chan_out_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_out_ps_net_or_captured_net,
        ce => ce_1112,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tuser_chan_out
    );
m_axis_data_tuser_chan_out_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_out_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1112, 
        o => m_axis_data_tuser_chan_out_ps_net_captured
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_1112,
        clr => \'0\',
        clk => clk_1112, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1112, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  cc_cmplr_v3_0_d31b0f3a494d423b_instance : cc_cmplr_v3_0_d31b0f3a494d423b
    port map(
      aclk=>clk,
      aclken=>ce,
      event_tlast_missing=>event_tlast_missing,
      event_tlast_unexpected=>event_tlast_unexpected,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tlast=>m_axis_data_tlast_ps_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tlast=>s_axis_data_tlast,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tvalid=>ce_logic_1
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_1112:in std_logic;
    ce_logic_1:in std_logic;
    clk:in std_logic;
    clk_1112:in std_logic;
    clk_logic_1:in std_logic;
    event_tlast_missing:out std_logic;
    event_tlast_unexpected:out std_logic;
    m_axis_data_tdata_data:out std_logic_vector(24 downto 0);
    m_axis_data_tlast:out std_logic;
    m_axis_data_tuser_chan_out:out std_logic_vector(0 downto 0);
    m_axis_data_tuser_chan_sync:out std_logic_vector(0 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata_data:in std_logic_vector(23 downto 0);
    s_axis_data_tlast:in std_logic;
    s_axis_data_tready:out std_logic
  );
end',
      'entity_name' => 'xlcic_compiler_afa2fe2f5db64f0b5e48b46aeb11f67e',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '9412f88e0627d2d077a93972d39808c4',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: unsigned((26 - 1) downto 0);
  signal output_port_5_5_force: signed((26 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_unsigned(input_port);
  output_port_5_5_force <= unsigned_to_signed(input_port_1_40);
  output_port <= signed_to_std_logic_vector(output_port_5_5_force);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((26 - 1) downto 0);
    output_port : out std_logic_vector((26 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_9934b94a22',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'dc173394b77b1f245adf4be5610d12ba',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlslice.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Multiplier virtex6 Xilinx,_Inc. 11.2',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET ccmimp = Distributed_Memory',
    'CSET clockenable = true',
    'CSET constvalue = 129',
    'CSET internaluser = 0',
    'CSET multiplier_construction = Use_LUTs',
    'CSET multtype = Parallel_Multiplier',
    'CSET optgoal = Speed',
    'CSET outputwidthhigh = 49',
    'CSET outputwidthlow = 0',
    'CSET pipestages = 8',
    'CSET portatype = Signed',
    'CSET portawidth = 25',
    'CSET portbtype = Signed',
    'CSET portbwidth = 25',
    'CSET roundpoint = 0',
    'CSET sclrcepriority = CE_Overrides_SCLR',
    'CSET syncclear = true',
    'CSET use_custom_output_width = true',
    'CSET userounding = false',
    'CSET zerodetect = false',
    'CSET component_name = mult_11_2_6d8e463c710483da',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '306cb4f5be6b5bace53c9734a52c06bd',
    'sourceFile' => 'hdl/xlmult.vhd',
    'templateKeyValues' => {
      'core_component_def' => '      b: in std_logic_vector(c_b_width - 1 downto 0);
      p: out std_logic_vector(c_output_width - 1 downto 0);
      clk: in std_logic;
      ce: in std_logic;
      sclr: in std_logic;
      a: in std_logic_vector(c_a_width - 1 downto 0)',
      'core_instance_text' => '        a => tmp_a,
        clk => clk,
        ce => internal_ce,
        sclr => internal_clr,
        p => tmp_p,
        b => tmp_b',
      'core_name0' => 'mult_11_2_6d8e463c710483da',
      'entity_name.0' => 'xlmult',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '4170f474aacf8c7783e26f8650e8e005',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ae6422a8c4120d5042e9c4785648960e',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlslice.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Multiplier virtex6 Xilinx,_Inc. 11.2',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET ccmimp = Distributed_Memory',
    'CSET clockenable = true',
    'CSET constvalue = 129',
    'CSET internaluser = 0',
    'CSET multiplier_construction = Use_LUTs',
    'CSET multtype = Parallel_Multiplier',
    'CSET optgoal = Speed',
    'CSET outputwidthhigh = 49',
    'CSET outputwidthlow = 0',
    'CSET pipestages = 8',
    'CSET portatype = Signed',
    'CSET portawidth = 25',
    'CSET portbtype = Signed',
    'CSET portbwidth = 25',
    'CSET roundpoint = 0',
    'CSET sclrcepriority = CE_Overrides_SCLR',
    'CSET syncclear = true',
    'CSET use_custom_output_width = true',
    'CSET userounding = false',
    'CSET zerodetect = false',
    'CSET component_name = mult_11_2_6d8e463c710483da',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'a8bcca57636bfff2e876c20df2a42e86',
    'sourceFile' => 'hdl/xlmult.vhd',
    'templateKeyValues' => {
      'core_component_def' => '      b: in std_logic_vector(c_b_width - 1 downto 0);
      p: out std_logic_vector(c_output_width - 1 downto 0);
      clk: in std_logic;
      ce: in std_logic;
      sclr: in std_logic;
      a: in std_logic_vector(c_a_width - 1 downto 0)',
      'core_instance_text' => '        a => tmp_a,
        clk => clk,
        ce => internal_ce,
        sclr => internal_clr,
        p => tmp_p,
        b => tmp_b',
      'core_name0' => 'mult_11_2_6d8e463c710483da',
      'entity_name.0' => 'xlmult',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Multiplier virtex6 Xilinx,_Inc. 11.2',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET ccmimp = Distributed_Memory',
    'CSET clockenable = true',
    'CSET constvalue = 129',
    'CSET internaluser = 0',
    'CSET multiplier_construction = Use_LUTs',
    'CSET multtype = Parallel_Multiplier',
    'CSET optgoal = Speed',
    'CSET outputwidthhigh = 49',
    'CSET outputwidthlow = 0',
    'CSET pipestages = 8',
    'CSET portatype = Signed',
    'CSET portawidth = 25',
    'CSET portbtype = Signed',
    'CSET portbwidth = 25',
    'CSET roundpoint = 0',
    'CSET sclrcepriority = CE_Overrides_SCLR',
    'CSET syncclear = true',
    'CSET use_custom_output_width = true',
    'CSET userounding = false',
    'CSET zerodetect = false',
    'CSET component_name = mult_11_2_6d8e463c710483da',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '6a06e9e0dd05cbd91841ddec1cb57ef9',
    'sourceFile' => 'hdl/xlmult.vhd',
    'templateKeyValues' => {
      'core_component_def' => '      b: in std_logic_vector(c_b_width - 1 downto 0);
      p: out std_logic_vector(c_output_width - 1 downto 0);
      clk: in std_logic;
      ce: in std_logic;
      sclr: in std_logic;
      a: in std_logic_vector(c_a_width - 1 downto 0)',
      'core_instance_text' => '        a => tmp_a,
        clk => clk,
        ce => internal_ce,
        sclr => internal_clr,
        p => tmp_p,
        b => tmp_b',
      'core_name0' => 'mult_11_2_6d8e463c710483da',
      'entity_name.0' => 'xlmult',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Complex_Multiplier virtex6 Xilinx,_Inc. 5.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET aclken = true',
    'CSET aportwidth = 24',
    'CSET aresetn = false',
    'CSET atuserwidth = 1',
    'CSET bportwidth = 24',
    'CSET btuserwidth = 1',
    'CSET ctrltuserwidth = 1',
    'CSET flowcontrol = NonBlocking',
    'CSET hasatlast = false',
    'CSET hasatuser = false',
    'CSET hasbtlast = false',
    'CSET hasbtuser = true',
    'CSET hasctrltlast = false',
    'CSET hasctrltuser = false',
    'CSET latencyconfig = Automatic',
    'CSET minimumlatency = 34',
    'CSET multtype = Use_Mults',
    'CSET optimizegoal = Performance',
    'CSET outputwidth = 24',
    'CSET outtlastbehv = Null',
    'CSET roundmode = Truncate',
    'CSET component_name = cmpy_v5_0_fc1d91881e8e8ae6',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '2a2c65b0a8593aecaaafd4121e65d821',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component cmpy_v5_0_fc1d91881e8e8ae6
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_dout_tdata:out std_logic_vector(47 downto 0);
      m_axis_dout_tuser:out std_logic_vector(0 downto 0);
      m_axis_dout_tvalid:out std_logic;
      s_axis_a_tdata:in std_logic_vector(47 downto 0);
      s_axis_a_tvalid:in std_logic;
      s_axis_b_tdata:in std_logic_vector(47 downto 0);
      s_axis_b_tuser:in std_logic_vector(0 downto 0);
      s_axis_b_tvalid:in std_logic
    );
end component;
signal m_axis_dout_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal s_axis_a_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal s_axis_b_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
begin
  m_axis_dout_tdata_imag <= m_axis_dout_tdata_net(47 downto 24);
  m_axis_dout_tdata_real <= m_axis_dout_tdata_net(23 downto 0);
  s_axis_a_tdata_net(47 downto 24) <= s_axis_a_tdata_imag;
  s_axis_a_tdata_net(23 downto 0) <= s_axis_a_tdata_real;
  s_axis_b_tdata_net(47 downto 24) <= s_axis_b_tdata_imag;
  s_axis_b_tdata_net(23 downto 0) <= s_axis_b_tdata_real;
  cmpy_v5_0_fc1d91881e8e8ae6_instance : cmpy_v5_0_fc1d91881e8e8ae6
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_dout_tdata=>m_axis_dout_tdata_net,
      m_axis_dout_tuser=>m_axis_dout_tuser,
      m_axis_dout_tvalid=>m_axis_dout_tvalid,
      s_axis_a_tdata=>s_axis_a_tdata_net,
      s_axis_a_tvalid=>s_axis_a_tvalid,
      s_axis_b_tdata=>s_axis_b_tdata_net,
      s_axis_b_tuser=>s_axis_b_tuser,
      s_axis_b_tvalid=>s_axis_b_tvalid
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    clk:in std_logic;
    m_axis_dout_tdata_imag:out std_logic_vector(23 downto 0);
    m_axis_dout_tdata_real:out std_logic_vector(23 downto 0);
    m_axis_dout_tuser:out std_logic_vector(0 downto 0);
    m_axis_dout_tvalid:out std_logic;
    s_axis_a_tdata_imag:in std_logic_vector(23 downto 0);
    s_axis_a_tdata_real:in std_logic_vector(23 downto 0);
    s_axis_a_tvalid:in std_logic;
    s_axis_b_tdata_imag:in std_logic_vector(23 downto 0);
    s_axis_b_tdata_real:in std_logic_vector(23 downto 0);
    s_axis_b_tuser:in std_logic_vector(0 downto 0);
    s_axis_b_tvalid:in std_logic
  );
end',
      'entity_name' => 'xlcomplex_multiplier_9420c9297365b1438cc1e8469b8205e1',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '08dd4ab2a51a26cb713887c189bcb357',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlconvert.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '7fd9f907230f07aa09e3f24dade6d949',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal d_1_22: std_logic_vector((24 - 1) downto 0);
begin
  d_1_22 <= d;
  q <= d_1_22;
end',
      'crippled_entity' => 'is
  port (
    d : in std_logic_vector((24 - 1) downto 0);
    q : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'delay_961b43f67a',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '6df3f26819c1199198075550e34ea901',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: unsigned((1 - 1) downto 0);
  signal b_1_34: unsigned((2 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (1 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    0 => false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal cast_12_12: unsigned((2 - 1) downto 0);
  signal result_12_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_unsigned(a);
  b_1_34 <= std_logic_vector_to_unsigned(b);
  op_mem_32_22_back <= op_mem_32_22(0);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  cast_12_12 <= u2u_cast(a_1_31, 0, 2, 0);
  result_12_3_rel <= cast_12_12 = b_1_34;
  op_mem_32_22_front_din <= result_12_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((1 - 1) downto 0);
    b : in std_logic_vector((2 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_d29d27b7b3',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '674fa2b0397629c3810995891e54b9ac',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "000000000000000000000000";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_f394f3309c',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '6eac3fcb48e2e71c2954ca4d25c4fadd',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlconvert.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '382aade0283634c192974b5423ff6719',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "11";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((2 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_3a9a3daeb9',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '5257a0eb5f203098dd35d83136a607ea',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "01";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((2 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_a7e2bb9e12',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b1f0ae968b381ba6ab9669a6ba2c3226',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
begin
  op <= "10";
end',
      'crippled_entity' => 'is
  port (
    op : out std_logic_vector((2 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'constant_e8ddc079e9',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '933f0a67148359c2b0b185c26938cbe3',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'a02a466d6ba04f2ec015247031d5f038',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: unsigned((2 - 1) downto 0);
  signal b_1_34: unsigned((2 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (1 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    0 => false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal result_12_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_unsigned(a);
  b_1_34 <= std_logic_vector_to_unsigned(b);
  op_mem_32_22_back <= op_mem_32_22(0);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  result_12_3_rel <= a_1_31 = b_1_34;
  op_mem_32_22_front_din <= result_12_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((2 - 1) downto 0);
    b : in std_logic_vector((2 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_367321bc0c',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '96225f22753efb24df501279341ff3a1',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '9fe6fa1103d94a931c682583a5597177',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'bdfed1e492741393e8cf68439d9d42db',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '2d0470915f6b2ce230a0e046e0e2f672',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '700476915c7b3b7728e9dcd7476094c3',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: unsigned((2 - 1) downto 0);
  signal b_1_34: unsigned((2 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (3 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    false,
    false,
    false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal result_12_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_unsigned(a);
  b_1_34 <= std_logic_vector_to_unsigned(b);
  op_mem_32_22_back <= op_mem_32_22(2);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        for i in 2 downto 1 loop 
          op_mem_32_22(i) <= op_mem_32_22(i-1);
        end loop;
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  result_12_3_rel <= a_1_31 = b_1_34;
  op_mem_32_22_front_din <= result_12_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((2 - 1) downto 0);
    b : in std_logic_vector((2 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_6c27b93bb1',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 15',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = 0.00054931640625,0.00125122070313,-0.0003662109375,-0.00466918945313,-0.00521850585938,0.00485229492188,0.0174255371094,0.0101318359375,-0.022705078125,-0.0442810058594,-0.00808715820313,0.0682373046875,0.0890197753906,-0.0151062011719,-0.166137695313,-0.173583984375,0.0633850097656,0.403045654297,0.564086914063,0.403045654297,0.0633850097656,-0.173583984375,-0.166137695313,-0.0151062011719,0.0890197753906,0.0682373046875,-0.00808715820313,-0.0442810058594,-0.022705078125,0.0101318359375,0.0174255371094,0.00485229492188,-0.00521850585938,-0.00466918945313,-0.0003662109375,0.00125122070313,0.00054931640625',
    'CSET columnconfig = 1',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 22',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 24',
    'CSET decimation_rate = 2',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Decimation',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 1390000',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Chan_ID_Field',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 4',
    'CSET number_paths = 1',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Hardware_Oversampling_Rate',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Chan_ID_Field',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 1390000',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_a41d007fd40e7497',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'f54b983d05b0a973fb91e07e57e37d5c',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_a41d007fd40e7497
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_s_data_chanid_incorrect:out std_logic;
      m_axis_data_tdata:out std_logic_vector(31 downto 0);
      m_axis_data_tuser:out std_logic_vector(1 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(23 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tuser:in std_logic_vector(1 downto 0);
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chanid_ps_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal s_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chanid_ps_net <= m_axis_data_tuser_net(1 downto 0);
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata;
  s_axis_data_tuser_net(1 downto 0) <= s_axis_data_tuser_chanid;
  m_axis_data_tdata_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_ps_net,
        ce => ce_2780000,
        clr => \'0\',
        clk => clk_2780000, 
        o => m_axis_data_tdata
    );
  m_axis_data_tuser_chanid_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 2,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chanid_ps_net,
        ce => ce_2780000,
        clr => \'0\',
        clk => clk_2780000, 
        o => m_axis_data_tuser_chanid
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_2780000,
        clr => \'0\',
        clk => clk_2780000, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_2780000, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_a41d007fd40e7497_instance : fr_cmplr_v6_3_a41d007fd40e7497
    port map(
      aclk=>clk,
      aclken=>ce,
      event_s_data_chanid_incorrect=>event_s_data_chanid_incorrect,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tuser=>s_axis_data_tuser_net,
      s_axis_data_tvalid=>ce_logic_1390000
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_1390000:in std_logic;
    ce_2780000:in std_logic;
    ce_logic_1390000:in std_logic;
    clk:in std_logic;
    clk_1390000:in std_logic;
    clk_2780000:in std_logic;
    clk_logic_1390000:in std_logic;
    event_s_data_chanid_incorrect:out std_logic;
    m_axis_data_tdata:out std_logic_vector(24 downto 0);
    m_axis_data_tuser_chanid:out std_logic_vector(1 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata:in std_logic_vector(23 downto 0);
    s_axis_data_tready:out std_logic;
    s_axis_data_tuser_chanid:in std_logic_vector(1 downto 0);
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_1dd9f35d7dcd9f893d99ee871ea21024',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT CIC_Compiler virtex6 Xilinx,_Inc. 3.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET clock_frequency = 200',
    'CSET differential_delay = 2',
    'CSET filter_type = Decimation',
    'CSET fixed_or_initial_rate = 2500',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 556',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET has_dout_tready = false',
    'CSET input_data_width = 24',
    'CSET input_sample_frequency = 0.00100000000',
    'CSET maximum_rate = 2500',
    'CSET minimum_rate = 2500',
    'CSET number_of_channels = 4',
    'CSET number_of_stages = 5',
    'CSET output_data_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET quantization = Truncation',
    'CSET ratespecification = Hardware_Oversampling_Rate',
    'CSET response_magnitude = Normalized',
    'CSET sample_rate_changes = Fixed',
    'CSET sampleperiod = 1',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET use_streaming_interface = true',
    'CSET use_xtreme_dsp_slice = true',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = cc_cmplr_v3_0_3ac902fec2d60a4a',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '9916bdfb71587a5e582923f0184bc3e5',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component cc_cmplr_v3_0_3ac902fec2d60a4a
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_tlast_missing:out std_logic;
      event_tlast_unexpected:out std_logic;
      m_axis_data_tdata:out std_logic_vector(31 downto 0);
      m_axis_data_tlast:out std_logic;
      m_axis_data_tuser:out std_logic_vector(15 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(23 downto 0);
      s_axis_data_tlast:in std_logic;
      s_axis_data_tready:out std_logic;
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net_captured: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_data_ps_net_or_captured_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tlast_ps_net: std_logic := \'0\';
signal m_axis_data_tlast_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tlast_ps_net_or_captured_net: std_logic := \'0\';
signal m_axis_data_tuser_net: std_logic_vector(15 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net_captured: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_sync_ps_net_or_captured_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net_captured: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chan_out_ps_net_or_captured_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(23 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_data_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chan_sync_ps_net <= m_axis_data_tuser_net(8 downto 8);
  m_axis_data_tuser_chan_out_ps_net <= m_axis_data_tuser_net(1 downto 0);
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata_data;
  m_axis_data_tdata_data_ps_net_or_captured_net <= m_axis_data_tdata_data_ps_net or m_axis_data_tdata_data_ps_net_captured;
m_axis_data_tdata_data_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_data_ps_net_or_captured_net,
        ce => ce_1390000,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tdata_data
    );
m_axis_data_tdata_data_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_data_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tdata_data_ps_net_captured
    );
  m_axis_data_tlast_ps_net_or_captured_net <= m_axis_data_tlast_ps_net or m_axis_data_tlast_ps_net_captured;
m_axis_data_tlast_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tlast_ps_net_or_captured_net,
        ce => ce_1390000,
        clr => \'0\',
        clk => clk_1390000, 
        o(0) => m_axis_data_tlast
    );
m_axis_data_tlast_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tlast_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1390000, 
        o(0) => m_axis_data_tlast_ps_net_captured
    );
  m_axis_data_tuser_chan_sync_ps_net_or_captured_net <= m_axis_data_tuser_chan_sync_ps_net or m_axis_data_tuser_chan_sync_ps_net_captured;
m_axis_data_tuser_chan_sync_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_sync_ps_net_or_captured_net,
        ce => ce_1390000,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tuser_chan_sync
    );
m_axis_data_tuser_chan_sync_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_sync_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tuser_chan_sync_ps_net_captured
    );
  m_axis_data_tuser_chan_out_ps_net_or_captured_net <= m_axis_data_tuser_chan_out_ps_net or m_axis_data_tuser_chan_out_ps_net_captured;
m_axis_data_tuser_chan_out_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 2,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_out_ps_net_or_captured_net,
        ce => ce_1390000,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tuser_chan_out
    );
m_axis_data_tuser_chan_out_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 2,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chan_out_ps_net,
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1390000, 
        o => m_axis_data_tuser_chan_out_ps_net_captured
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_1390000,
        clr => \'0\',
        clk => clk_1390000, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_1390000, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  cc_cmplr_v3_0_3ac902fec2d60a4a_instance : cc_cmplr_v3_0_3ac902fec2d60a4a
    port map(
      aclk=>clk,
      aclken=>ce,
      event_tlast_missing=>event_tlast_missing,
      event_tlast_unexpected=>event_tlast_unexpected,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tlast=>m_axis_data_tlast_ps_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tlast=>s_axis_data_tlast,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tvalid=>ce_logic_556
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_1390000:in std_logic;
    ce_556:in std_logic;
    ce_logic_556:in std_logic;
    clk:in std_logic;
    clk_1390000:in std_logic;
    clk_556:in std_logic;
    clk_logic_556:in std_logic;
    event_tlast_missing:out std_logic;
    event_tlast_unexpected:out std_logic;
    m_axis_data_tdata_data:out std_logic_vector(24 downto 0);
    m_axis_data_tlast:out std_logic;
    m_axis_data_tuser_chan_out:out std_logic_vector(1 downto 0);
    m_axis_data_tuser_chan_sync:out std_logic_vector(0 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata_data:in std_logic_vector(23 downto 0);
    s_axis_data_tlast:in std_logic;
    s_axis_data_tready:out std_logic
  );
end',
      'entity_name' => 'xlcic_compiler_f8e45ed45c0be88ce1d78d69ce0c1bbd',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 15',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = -3.0517578125e-005,-0.00030517578125,0.000274658203125,0.000244140625,-0.000518798828125,-0.00018310546875,0.0008544921875,-3.0517578125e-005,-0.00128173828125,0.00048828125,0.00167846679688,-0.00125122070313,-0.00189208984375,0.00234985351563,0.00177001953125,-0.00372314453125,-0.00115966796875,0.00527954101563,-0.000213623046875,-0.00680541992188,0.00247192382813,0.0079345703125,-0.00570678710938,-0.00830078125,0.0098876953125,0.00741577148438,-0.0149536132813,-0.00469970703125,0.0205993652344,-0.000518798828125,-0.0264892578125,0.00924682617188,0.0322265625,-0.0231628417969,-0.0373229980469,0.0465698242188,0.0413208007813,-0.0953369140625,-0.0438842773438,0.314636230469,0.544769287109,0.314636230469,-0.0438842773438,-0.0953369140625,0.0413208007813,0.0465698242188,-0.0373229980469,-0.0231628417969,0.0322265625,0.00924682617188,-0.0264892578125,-0.000518798828125,0.0205993652344,-0.00469970703125,-0.0149536132813,0.00741577148438,0.0098876953125,-0.00830078125,-0.00570678710938,0.0079345703125,0.00247192382813,-0.00680541992188,-0.000213623046875,0.00527954101563,-0.00115966796875,-0.00372314453125,0.00177001953125,0.00234985351563,-0.00189208984375,-0.00125122070313,0.00167846679688,0.00048828125,-0.00128173828125,-3.0517578125e-005,0.0008544921875,-0.00018310546875,-0.000518798828125,0.000244140625,0.000274658203125,-0.00030517578125,-3.0517578125e-005',
    'CSET columnconfig = 1',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 22',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 24',
    'CSET decimation_rate = 2',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Decimation',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 2780000',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Chan_ID_Field',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 4',
    'CSET number_paths = 1',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Hardware_Oversampling_Rate',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Chan_ID_Field',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 2780000',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_427173487ade39a1',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '547c30c9566276e6f6a9131dee7f286c',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_427173487ade39a1
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_s_data_chanid_incorrect:out std_logic;
      m_axis_data_tdata:out std_logic_vector(31 downto 0);
      m_axis_data_tuser:out std_logic_vector(1 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(23 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tuser:in std_logic_vector(1 downto 0);
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chanid_ps_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal s_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chanid_ps_net <= m_axis_data_tuser_net(1 downto 0);
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata;
  s_axis_data_tuser_net(1 downto 0) <= s_axis_data_tuser_chanid;
  m_axis_data_tdata_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_ps_net,
        ce => ce_5560000,
        clr => \'0\',
        clk => clk_5560000, 
        o => m_axis_data_tdata
    );
  m_axis_data_tuser_chanid_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 2,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chanid_ps_net,
        ce => ce_5560000,
        clr => \'0\',
        clk => clk_5560000, 
        o => m_axis_data_tuser_chanid
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_5560000,
        clr => \'0\',
        clk => clk_5560000, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_5560000, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_427173487ade39a1_instance : fr_cmplr_v6_3_427173487ade39a1
    port map(
      aclk=>clk,
      aclken=>ce,
      event_s_data_chanid_incorrect=>event_s_data_chanid_incorrect,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tuser=>s_axis_data_tuser_net,
      s_axis_data_tvalid=>ce_logic_2780000
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_2780000:in std_logic;
    ce_5560000:in std_logic;
    ce_logic_2780000:in std_logic;
    clk:in std_logic;
    clk_2780000:in std_logic;
    clk_5560000:in std_logic;
    clk_logic_2780000:in std_logic;
    event_s_data_chanid_incorrect:out std_logic;
    m_axis_data_tdata:out std_logic_vector(24 downto 0);
    m_axis_data_tuser_chanid:out std_logic_vector(1 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata:in std_logic_vector(23 downto 0);
    s_axis_data_tready:out std_logic;
    s_axis_data_tuser_chanid:in std_logic_vector(1 downto 0);
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_89a8e066ed5ac2da6aa25a1c95609ee1',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '2530f8df112851d515ce162394e0a81b',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: unsigned((24 - 1) downto 0);
  signal output_port_5_5_force: signed((24 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_unsigned(input_port);
  output_port_5_5_force <= unsigned_to_signed(input_port_1_40);
  output_port <= signed_to_std_logic_vector(output_port_5_5_force);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((24 - 1) downto 0);
    output_port : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_4bf1ad328a',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'f6da37858f52b62562fa3360e4b0ae08',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlslice.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '86aa8c10a9c51467c0bffd3f7fe585fa',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '0192b1127bceeb79355a1415d8db1e02',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 20',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = -3.19881109434e-005,-3.34757195765e-005,-3.48867711839e-005,-3.62107041422e-005,-3.74369770819e-005,-3.85551197684e-005,-3.95547851252e-005,-4.04258021945e-005,-4.11582298157e-005,-4.17424108002e-005,-4.2169026368e-005,-4.24291506081e-005,-4.25143047185e-005,-4.24165107753e-005,-4.21283447776e-005,-4.16429887131e-005,-4.09542813842e-005,-4.00567677386e-005,-3.89457464435e-005,-3.76173154487e-005,-3.6068415283e-005,-3.42968698351e-005,-3.23014243728e-005,-3.00817805621e-005,-2.76386282547e-005,-2.49736738206e-005,-2.20896648129e-005,-1.89904107612e-005,-1.5680799905e-005,-1.21668116873e-005,-8.45552484614e-006,-4.55512095526e-006,-4.74883281192e-007,3.77480915869e-006,8.18249243499e-006,1.27356335212e-005,1.74206511449e-005,2.22229417145e-005,2.71269103415e-005,3.21160069563e-005,3.71727674939e-005,4.2278860104e-005,4.7415136318e-005,5.2561687084e-005,5.76979035543e-005,6.28025424934e-005,6.78537961464e-005,7.28293663896e-005,7.77065429591e-005,8.24622855355e-005,8.7073309439e-005,9.15161746683e-005,9.57673779991e-005,9.98034478356e-005,0.000103601041492,0.000107137044566,0.000110388672039,0.000113333570738,0.000115949922773,0.000118216549539,0.000120113015884,0.000121619734017,0.000122718066721,0.00012339042944,0.000123620390801,0.000123392771107,0.000122693738384,0.000121510901496,0.000119833399919,0.000117651989696,0.000114959125164,0.000111749036008,0.000108017799216,0.000103763405539,9.89858200459e-005,9.3687036395e-005,8.78711244581e-005,8.15442709421e-005,7.47148126866e-005,6.73932623281e-005,5.95923260492e-005,5.13269131541e-005,4.26141372411e-005,3.34733087689e-005,2.39259188422e-005,1.39956140765e-005,3.70816242985e-006,-6.90859007421e-006,-1.78247717771e-005,-2.90085469756e-005,-4.04261866463e-005,-5.2042148238e-005,-6.38191644344e-005,-7.57183407459e-005,-8.76992617574e-005,-9.97201058186e-005,-0.000111737767927,-0.000123707990514,-0.000135585501816,-0.00014732416146,-0.000158877112879,-0.000170196942118,-0.000181235842566,-0.000191945785124,-0.000202278693274,-0.000212186622487,-0.000221621943391,-0.000230537528077,-0.000238886938914,-0.000246624619191,-0.000253706084928,-0.000260088117138,-0.000265728953823,-0.000270588480973,-0.000274628421835,-0.000277812523675,-0.000280106741305,-0.000281479416585,-0.000281901453154,-0.000281346485627,-0.000279791042494,-0.00027721470199,-0.000273600240182,-0.000268933770568,-0.000263204874469,-0.000256406721545,-0.000248536179754,-0.000239593914144,-0.000229584473848,-0.000218516366723,-0.000206402121086,-0.000193258334046,-0.000179105705971,-0.000163969060666,-0.000147877350891,-0.000130863648878,-0.000112965121588,-9.42229904531e-005,-7.46824754472e-005,-5.43927233552e-005,-3.34067201729e-005,-1.17811876324e-005,1.0423536101e-005,3.31436314498e-005,5.6311947997e-005,7.98581703813e-005,0.000103708999225,0.000127788346748,0.000152017546652,0.000176315577814,0.000200599301246,0.000224783709749,0.000248782189601,0.000272506793583,0.000295868524592,0.000318777629016,0.000341143899029,0.000362876982883,0.000383886702248,0.000404083375596,0.000423378146597,0.000441683316437,0.000458912678965,0.000474981857497,0.000489808642138,0.000503313326401,0.000515419041924,0.000526052090047,0.000535142269011,0.000542623195519,0.000548432619427,0.000552512730283,0.000554810454496,0.00055527774188,0.000553871840366,0.000550555557665,0.000545297508722,0.000538072347796,0.000528860984064,0.000517650779666,0.000504435729154,0.000489216619361,0.00047200116875,0.000452804145366,0.000431647462562,0.000408560251742,0.000383578911433,0.000356747132061,0.000328115895878,0.000297743451581,0.000265695263223,0.000232043933115,0.000196869098485,0.000160257301782,0.000122301834545,8.31025549253e-005,4.27656789541e-005,1.40354582766e-006,-4.08656424913e-005,-8.39181069241e-005,-0.000127624801666,-0.000171851748706,-0.000216460395772,-0.000261307996909,-0.000306248014796,-0.000351130543825,-0.000395802752877,-0.000440109346648,-0.000483893044267,-0.00052699507392,-0.000569255682056,-0.00061051465573,-0.000650611856528,-0.000689387764481,-0.000726684030297,-0.000762344034189,-0.000796213449529,-0.000828140809507,-0.000857978074927,-0.000885581201251,-0.000910810702967,-0.000933532213318,-0.000953617037449,-0.000970942696964,-0.000985393463943,-0.000996860882419,-0.00100524427535,-0.00101045123514,-0.00101239809576,-0.00101101038462,-0.00100622325219,-0.000997981877792,-0.00098624184948,-0.000970969516587,-0.000952142313087,-0.000929749050275,-0.000903790177244,-0.00087427800773,-0.000841236911994,-0.000804703472494,-0.000764726602207,-0.00072136762456,-0.000674700314047,-0.000624810896711,-0.000571798009798,-0.00051577262001,-0.000456857899905,-0.000395189062142,-0.000330913151362,-0.000264188793675,-0.00019518590383,-0.000124085350303,-5.10785786472e-005,2.36328063536e-005,9.98374991174e-005,0.000177314991468,0.000255836109695,0.000335163589396,0.000415052686902,0.000495251825934,0.000575503278033,0.000655543875137,0.000735105752601,0.000813917120774,0.000891703063193,0.000968186359282,0.00104308832937,0.00111612969973,0.00118703148527,0.00125551588728,0.00132130720389,0.00138413275029,0.00144372378628,0.00149981644818,0.00155215268241,0.00160048117767,0.00164455829305,0.00168414897896,0.00171902768796,0.00174897927255,0.0017737998669,0.00179329774965,0.00180729418468,0.00181562423715,0.00181813756171,0.00181469916022,0.00180519010612,0.00178950823275,0.00176756878302,0.00173930501788,0.00170466878101,0.00166363101764,0.00161618224495,0.00156233297216,0.00150211406818,0.00143557707502,0.00136279446515,0.00128385984141,0.00119888807785,0.00110801540036,0.00101139940594,0.000909219019783,0.000801674389323,0.000688986714711,0.000571398015399,0.000449170832582,0.000322587867524,0.000191951555961,5.75835789596e-005,-8.01756891692e-005,-0.000220967795148,-0.000364416882207,-0.000510130425609,-0.000657700026849,-0.000806702265003,-0.000956699603466,-0.00110724135019,-0.00125786466929,-0.00140809564181,-0.00155745037308,-0.00170543614427,-0.00185155260508,-0.00199529300498,-0.00213614545972,-0.00227359424991,-0.00240712114854,-0.00253620677379,-0.00266033196372,-0.00277897916908,-0.00289163386066,-0.00299778594719,-0.00309693119999,-0.00318857268048,-0.00327222216642,-0.00334740157302,-0.00341364436477,-0.0034704969539,-0.0035175200816,-0.00355429017771,-0.00358040069503,-0.0035954634143,-0.0035991097157,-0.00359099181327,-0.0035707839483,-0.00353818353795,-0.00349291227566,-0.00343471717962,-0.0033633715861,-0.0032786760843,-0.00318045938953,-0.003068579152,-0.00294292269809,-0.00280340770169,-0.00264998278317,-0.00248262803346,-0.0023013554615,-0.00210620936297,-0.00189726660862,-0.00167463685095,-0.00143846264778,-0.00118891950191,-0.000926215815895,-0.000650592761623,-0.000362324064186,-6.17157001332e-005,0.000250894489805,0.000575137273058,0.000910612581976,0.00125689015411,0.00161351025487,0.00197998448119,0.00235579664452,0.00274040373148,0.00313323693984,0.00353370278783,0.00394118429397,0.00435504222498,0.00477461640862,0.00519922710855,0.00562817645768,0.00606074994674,0.00649621796423,0.00693383738414,0.00737285319725,0.00781250018209,0.00825200461121,0.00869058598857,0.00912745881335,0.00956183436592,0.00999292251118,0.0104199335146,0.0108420798663,0.0112585781083,0.0116686506603,0.012071527639,0.012466448666,0.0128526646603,0.0132294396093,0.0135960523151,0.0139517981098,0.014295990537,0.0146279629933,0.0149470703265,0.0152526903859,0.0155442255204,0.0158211040203,0.0160827814991,0.0163287422115,0.0165585003042,0.0167716009957,0.0169676216817,0.0171461729647,0.0173068996023,0.0174494813744,0.0175736338647,0.0176791091561,0.0177656964365,0.0178332225151,0.0178815522458,0.0179105888587,0.0179202741969,0.0179105888587,0.0178815522458,0.0178332225151,0.0177656964365,0.0176791091561,0.0175736338647,0.0174494813744,0.0173068996023,0.0171461729647,0.0169676216817,0.0167716009957,0.0165585003042,0.0163287422115,0.0160827814991,0.0158211040203,0.0155442255204,0.0152526903859,0.0149470703265,0.0146279629933,0.014295990537,0.0139517981098,0.0135960523151,0.0132294396093,0.0128526646603,0.012466448666,0.012071527639,0.0116686506603,0.0112585781083,0.0108420798663,0.0104199335146,0.00999292251118,0.00956183436592,0.00912745881335,0.00869058598857,0.00825200461121,0.00781250018209,0.00737285319725,0.00693383738414,0.00649621796423,0.00606074994674,0.00562817645768,0.00519922710855,0.00477461640862,0.00435504222498,0.00394118429397,0.00353370278783,0.00313323693984,0.00274040373148,0.00235579664452,0.00197998448119,0.00161351025487,0.00125689015411,0.000910612581976,0.000575137273058,0.000250894489805,-6.17157001332e-005,-0.000362324064186,-0.000650592761623,-0.000926215815895,-0.00118891950191,-0.00143846264778,-0.00167463685095,-0.00189726660862,-0.00210620936297,-0.0023013554615,-0.00248262803346,-0.00264998278317,-0.00280340770169,-0.00294292269809,-0.003068579152,-0.00318045938953,-0.0032786760843,-0.0033633715861,-0.00343471717962,-0.00349291227566,-0.00353818353795,-0.0035707839483,-0.00359099181327,-0.0035991097157,-0.0035954634143,-0.00358040069503,-0.00355429017771,-0.0035175200816,-0.0034704969539,-0.00341364436477,-0.00334740157302,-0.00327222216642,-0.00318857268048,-0.00309693119999,-0.00299778594719,-0.00289163386066,-0.00277897916908,-0.00266033196372,-0.00253620677379,-0.00240712114854,-0.00227359424991,-0.00213614545972,-0.00199529300498,-0.00185155260508,-0.00170543614427,-0.00155745037308,-0.00140809564181,-0.00125786466929,-0.00110724135019,-0.000956699603466,-0.000806702265003,-0.000657700026849,-0.000510130425609,-0.000364416882207,-0.000220967795148,-8.01756891692e-005,5.75835789596e-005,0.000191951555961,0.000322587867524,0.000449170832582,0.000571398015399,0.000688986714711,0.000801674389323,0.000909219019783,0.00101139940594,0.00110801540036,0.00119888807785,0.00128385984141,0.00136279446515,0.00143557707502,0.00150211406818,0.00156233297216,0.00161618224495,0.00166363101764,0.00170466878101,0.00173930501788,0.00176756878302,0.00178950823275,0.00180519010612,0.00181469916022,0.00181813756171,0.00181562423715,0.00180729418468,0.00179329774965,0.0017737998669,0.00174897927255,0.00171902768796,0.00168414897896,0.00164455829305,0.00160048117767,0.00155215268241,0.00149981644818,0.00144372378628,0.00138413275029,0.00132130720389,0.00125551588728,0.00118703148527,0.00111612969973,0.00104308832937,0.000968186359282,0.000891703063193,0.000813917120774,0.000735105752601,0.000655543875137,0.000575503278033,0.000495251825934,0.000415052686902,0.000335163589396,0.000255836109695,0.000177314991468,9.98374991174e-005,2.36328063536e-005,-5.10785786472e-005,-0.000124085350303,-0.00019518590383,-0.000264188793675,-0.000330913151362,-0.000395189062142,-0.000456857899905,-0.00051577262001,-0.000571798009798,-0.000624810896711,-0.000674700314047,-0.00072136762456,-0.000764726602207,-0.000804703472494,-0.000841236911994,-0.00087427800773,-0.000903790177244,-0.000929749050275,-0.000952142313087,-0.000970969516587,-0.00098624184948,-0.000997981877792,-0.00100622325219,-0.00101101038462,-0.00101239809576,-0.00101045123514,-0.00100524427535,-0.000996860882419,-0.000985393463943,-0.000970942696964,-0.000953617037449,-0.000933532213318,-0.000910810702967,-0.000885581201251,-0.000857978074927,-0.000828140809507,-0.000796213449529,-0.000762344034189,-0.000726684030297,-0.000689387764481,-0.000650611856528,-0.00061051465573,-0.000569255682056,-0.00052699507392,-0.000483893044267,-0.000440109346648,-0.000395802752877,-0.000351130543825,-0.000306248014796,-0.000261307996909,-0.000216460395772,-0.000171851748706,-0.000127624801666,-8.39181069241e-005,-4.08656424913e-005,1.40354582766e-006,4.27656789541e-005,8.31025549253e-005,0.000122301834545,0.000160257301782,0.000196869098485,0.000232043933115,0.000265695263223,0.000297743451581,0.000328115895878,0.000356747132061,0.000383578911433,0.000408560251742,0.000431647462562,0.000452804145366,0.00047200116875,0.000489216619361,0.000504435729154,0.000517650779666,0.000528860984064,0.000538072347796,0.000545297508722,0.000550555557665,0.000553871840366,0.00055527774188,0.000554810454496,0.000552512730283,0.000548432619427,0.000542623195519,0.000535142269011,0.000526052090047,0.000515419041924,0.000503313326401,0.000489808642138,0.000474981857497,0.000458912678965,0.000441683316437,0.000423378146597,0.000404083375596,0.000383886702248,0.000362876982883,0.000341143899029,0.000318777629016,0.000295868524592,0.000272506793583,0.000248782189601,0.000224783709749,0.000200599301246,0.000176315577814,0.000152017546652,0.000127788346748,0.000103708999225,7.98581703813e-005,5.6311947997e-005,3.31436314498e-005,1.0423536101e-005,-1.17811876324e-005,-3.34067201729e-005,-5.43927233552e-005,-7.46824754472e-005,-9.42229904531e-005,-0.000112965121588,-0.000130863648878,-0.000147877350891,-0.000163969060666,-0.000179105705971,-0.000193258334046,-0.000206402121086,-0.000218516366723,-0.000229584473848,-0.000239593914144,-0.000248536179754,-0.000256406721545,-0.000263204874469,-0.000268933770568,-0.000273600240182,-0.00027721470199,-0.000279791042494,-0.000281346485627,-0.000281901453154,-0.000281479416585,-0.000280106741305,-0.000277812523675,-0.000274628421835,-0.000270588480973,-0.000265728953823,-0.000260088117138,-0.000253706084928,-0.000246624619191,-0.000238886938914,-0.000230537528077,-0.000221621943391,-0.000212186622487,-0.000202278693274,-0.000191945785124,-0.000181235842566,-0.000170196942118,-0.000158877112879,-0.00014732416146,-0.000135585501816,-0.000123707990514,-0.000111737767927,-9.97201058186e-005,-8.76992617574e-005,-7.57183407459e-005,-6.38191644344e-005,-5.2042148238e-005,-4.04261866463e-005,-2.90085469756e-005,-1.78247717771e-005,-6.90859007421e-006,3.70816242985e-006,1.39956140765e-005,2.39259188422e-005,3.34733087689e-005,4.26141372411e-005,5.13269131541e-005,5.95923260492e-005,6.73932623281e-005,7.47148126866e-005,8.15442709421e-005,8.78711244581e-005,9.3687036395e-005,9.89858200459e-005,0.000103763405539,0.000108017799216,0.000111749036008,0.000114959125164,0.000117651989696,0.000119833399919,0.000121510901496,0.000122693738384,0.000123392771107,0.000123620390801,0.00012339042944,0.000122718066721,0.000121619734017,0.000120113015884,0.000118216549539,0.000115949922773,0.000113333570738,0.000110388672039,0.000107137044566,0.000103601041492,9.98034478356e-005,9.57673779991e-005,9.15161746683e-005,8.7073309439e-005,8.24622855355e-005,7.77065429591e-005,7.28293663896e-005,6.78537961464e-005,6.28025424934e-005,5.76979035543e-005,5.2561687084e-005,4.7415136318e-005,4.2278860104e-005,3.71727674939e-005,3.21160069563e-005,2.71269103415e-005,2.22229417145e-005,1.74206511449e-005,1.27356335212e-005,8.18249243499e-006,3.77480915869e-006,-4.74883281192e-007,-4.55512095526e-006,-8.45552484614e-006,-1.21668116873e-005,-1.5680799905e-005,-1.89904107612e-005,-2.20896648129e-005,-2.49736738206e-005,-2.76386282547e-005,-3.00817805621e-005,-3.23014243728e-005,-3.42968698351e-005,-3.6068415283e-005,-3.76173154487e-005,-3.89457464435e-005,-4.00567677386e-005,-4.09542813842e-005,-4.16429887131e-005,-4.21283447776e-005,-4.24165107753e-005,-4.25143047185e-005,-4.24291506081e-005,-4.2169026368e-005,-4.17424108002e-005,-4.11582298157e-005,-4.04258021945e-005,-3.95547851252e-005,-3.85551197684e-005,-3.74369770819e-005,-3.62107041422e-005,-3.48867711839e-005,-3.34757195765e-005,-3.19881109434e-005',
    'CSET columnconfig = 13',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 22',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 24',
    'CSET decimation_rate = 35',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Decimation',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 2780000',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Chan_ID_Field',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 2',
    'CSET number_paths = 2',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Maximum_Possible',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Chan_ID_Field',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 1',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_518400b39fb6cdb9',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ac0363a16fc94acdc8ab6c3aa8138e93',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_518400b39fb6cdb9
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_s_data_chanid_incorrect:out std_logic;
      m_axis_data_tdata:out std_logic_vector(63 downto 0);
      m_axis_data_tuser:out std_logic_vector(0 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(47 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tuser:in std_logic_vector(0 downto 0);
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(63 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path1_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path0_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chanid_ps_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal s_axis_data_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_path1_ps_net <= m_axis_data_tdata_net(56 downto 32);
  m_axis_data_tdata_path0_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chanid_ps_net <= m_axis_data_tuser_net(0 downto 0);
  s_axis_data_tdata_net(47 downto 24) <= s_axis_data_tdata_path1;
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata_path0;
  s_axis_data_tuser_net(0 downto 0) <= s_axis_data_tuser_chanid;
  m_axis_data_tdata_path1_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path1_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tdata_path1
    );
  m_axis_data_tdata_path0_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path0_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tdata_path0
    );
  m_axis_data_tuser_chanid_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chanid_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tuser_chanid
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_35, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_518400b39fb6cdb9_instance : fr_cmplr_v6_3_518400b39fb6cdb9
    port map(
      aclk=>clk,
      aclken=>ce,
      event_s_data_chanid_incorrect=>event_s_data_chanid_incorrect,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tuser=>s_axis_data_tuser_net,
      s_axis_data_tvalid=>ce_logic_1
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_35:in std_logic;
    ce_logic_1:in std_logic;
    clk:in std_logic;
    clk_35:in std_logic;
    clk_logic_1:in std_logic;
    event_s_data_chanid_incorrect:out std_logic;
    m_axis_data_tdata_path0:out std_logic_vector(24 downto 0);
    m_axis_data_tdata_path1:out std_logic_vector(24 downto 0);
    m_axis_data_tuser_chanid:out std_logic_vector(0 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata_path0:in std_logic_vector(23 downto 0);
    s_axis_data_tdata_path1:in std_logic_vector(23 downto 0);
    s_axis_data_tready:out std_logic;
    s_axis_data_tuser_chanid:in std_logic_vector(0 downto 0);
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_6d145a00472f9003e5f9a2aeba87666d',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 20',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = -3.1988110943394458e-005,-3.3475719576492922e-005,-3.488677118385603e-005,-3.6210704142163814e-005,-3.7436977081942656e-005,-3.8555119768387664e-005,-3.9554785125218651e-005,-4.0425802194468398e-005,-4.1158229815698028e-005,-4.1742410800231418e-005,-4.2169026367988538e-005,-4.2429150608084892e-005,-4.2514304718516722e-005,-4.241651077529046e-005,-4.2128344777641997e-005,-4.1642988713078401e-005,-4.0954281384187789e-005,-4.0056767738570597e-005,-3.8945746443507503e-005,-3.7617315448704588e-005,-3.6068415283027002e-005,-3.4296869835144618e-005,-3.2301424372823877e-005,-3.0081780562104175e-005,-2.7638628254697786e-005,-2.4973673820632981e-005,-2.208966481285418e-005,-1.8990410761158292e-005,-1.5680799904973384e-005,-1.2166811687281863e-005,-8.4555248461350258e-006,-4.5551209552641986e-006,-4.748832811923351e-007,3.7748091586949831e-006,8.1824924349908408e-006,1.2735633521231706e-005,1.7420651144911701e-005,2.2222941714456671e-005,2.7126910341456481e-005,3.211600695631732e-005,3.7172767493934646e-005,4.2278860103952291e-005,4.7415136318033758e-005,5.2561687084014779e-005,5.7697903554303826e-005,6.2802542493354844e-005,6.7853796146420507e-005,7.2829366389609831e-005,7.7706542959057819e-005,8.2462285535489524e-005,8.7073309438981164e-005,9.1516174668330197e-005,9.5767377999136677e-005,9.980344783558122e-005,0.00010360104149247878,0.00010713704456644734,0.00011038867203894197,0.00011333357073815553,0.00011594992277288528,0.00011821654953855146,0.00012011301588390767,0.00012161973401708254,0.00012271806672090166,0.00012339042944046357,0.00012362039080059407,0.00012339277110709677,0.00012269373838366836,0.00012151090149635649,0.000119833399918863,0.00011765198969552485,0.00011495912516417553,0.00011174903600823584,0.00010801779921638294,0.00010376340553933867,9.8985820045867232e-005,9.3687036395020352e-005,8.7871124458095901e-005,8.1544270942065337e-005,7.4714812686558853e-005,6.7393262328148919e-005,5.959232604923072e-005,5.1326913154074413e-005,4.2614137241134129e-005,3.3473308768895735e-005,2.3925918842185296e-005,1.399561407645275e-005,3.7081624298488197e-006,-6.9085900742068874e-006,-1.7824771777069012e-005,-2.9008546975628897e-005,-4.0426186646250175e-005,-5.2042148238008166e-005,-6.3819164434437753e-005,-7.5718340745911544e-005,-8.7699261757374472e-005,-9.9720105818573333e-005,-0.00011173776792668028,-0.00012370799051387895,-0.0001355855018157555,-0.00014732416146003336,-0.00015887711287947752,-0.00017019694211808706,-0.00018123584256579615,-0.00019194578512417325,-0.00020227869327419507,-0.00021218662248708597,-0.00022162194339074567,-0.00023053752807749851,-0.00023888693891392331,-0.00024662461919051372,-0.00025370608492800752,-0.00026008811713849791,-0.00026572895382303993,-0.00027058848097346298,-0.00027462842183468704,-0.00027781252367493008,-0.00028010674130506043,-0.00028147941658498082,-0.00028190145315427028,-0.00028134648562676393,-0.00027979104249386351,-0.00027721470198962622,-0.00027360024018187925,-0.00026893377056777621,-0.00026320487446929755,-0.00025640672154453011,-0.00024853617975352944,-0.00023959391414360759,-0.0002295844738479997,-0.00021851636672336324,-0.00020640212108628597,-0.0001932583340460852,-0.00017910570597086024,-0.00016396906066624842,-0.00014787735089073669,-0.00013086364887831731,-0.00011296512158832281,-9.4222990453146521e-005,-7.468247544720144e-005,-5.439272335515868e-005,-3.3406720172927598e-005,-1.1781187632420663e-005,1.0423536100950135e-005,3.31436314497614e-005,5.631194799704007e-005,7.9858170381307512e-005,0.00010370899922518493,0.00012778834674800928,0.00015201754665230622,0.00017631557781390582,0.00020059930124605364,0.00022478370974925876,0.00024878218960101893,0.00027250679358344331,0.00029586852459198614,0.00031877762901577487,0.00034114389902887882,0.00036287698288332094,0.00038388670224828996,0.00040408337559634581,0.00042337814659663123,0.00044168331643729537,0.000458912678964864,0.00047498185749703369,0.0004898086421379151,0.00050331332640081439,0.00051541904192377883,0.00052605209004715316,0.00053514226901057668,0.00054262319551943007,0.00054843261942740678,0.00055251273028330476,0.00055481045449575904,0.00055527774188004629,0.0005538718403659802,0.00055055555766543823,0.00054529750872225077,0.00053807234779593258,0.00052886098406408188,0.00051765077966621422,0.000504435729154112,0.00048921661936057476,0.00047200116874952889,0.00045280414536572039,0.00043164746256151048,0.00040856025174159277,0.0003835789114333784,0.00035674713206145615,0.00032811589587836045,0.00029774345158117832,0.00026569526322344774,0.00023204393311471031,0.00019686909848531608,0.00016025730178152766,0.0001223018345454641,8.3102554925345751e-005,4.2765678954100008e-005,1.4035458276572845e-006,-4.0865642491326196e-005,-8.3918106924121865e-005,-0.00012762480166591899,-0.00017185174870559347,-0.00021646039577189773,-0.00026130799690945073,-0.00030624801479634233,-0.00035113054382468478,-0.00039580275287734901,-0.00044010934664787918,-0.0004838930442671783,-0.00052699507391989206,-0.00056925568205609569,-0.00061051465573013961,-0.00065061185652830786,-0.00068938776448143548,-0.00072668403029697989,-0.00076234403418854,-0.00079621344952903047,-0.00082814080950729702,-0.00085797807492699225,-0.00088558120125116024,-0.00091081070296653683,-0.00093353221331832854,-0.0009536170374490337,-0.00097094269696413064,-0.00098539346394335291,-0.00099686088241859698,-0.0010052442753487613,-0.0010104512351375027,-0.0010123980957629612,-0.0010110103846177473,-0.0010062232521940971,-0.0009979818777921957,-0.00098624184947972517,-0.00097096951658728176,-0.00095214231308748108,-0.00092974905027540754,-0.00090379017724398264,-0.00087427800772989803,-0.00084123691199406738,-0.0008047034724942444,-0.00076472660220685521,-0.00072136762455972435,-0.00067470031404693502,-0.00062481089671129176,-0.0005717980097984704,-0.00051577262000956187,-0.00045685789990494035,-0.00039518906214202178,-0.00033091315136185117,-0.00026418879367451001,-0.00019518590383031607,-0.000124085350302521,-5.1078578647156268e-005,2.3632806353627878e-005,9.9837499117379398e-005,0.00017731499146802959,0.00025583610969489283,0.00033516358939623833,0.00041505268690199612,0.00049525182593448817,0.00057550327803295657,0.00065554387513742289,0.00073510575260060391,0.00081391712077365577,0.00089170306319302762,0.00096818635928169392,0.0010430883293695205,0.0011161296997343484,0.0011870314852684863,0.001255515887284487,0.0013213072038903552,0.0013841327502874465,0.0014437237862751074,0.00149981644818455,0.0015521526824108444,0.0016004811776669753,0.0016445582930469849,0.0016841489789577672,0.0017190276879598323,0.0017489792725479184,0.0017737998669017623,0.0017932977496462304,0.0018072941846783406,0.0018156242371466917,0.00181813756170604,0.0018146991602167695,0.0018051901061150568,0.0017895082327456444,0.0017675687830236559,0.0017393050178763109,0.0017046687810081695,0.0016636310176356666,0.0016161822449466874,0.0015623329721597911,0.0015021140681841872,0.001435577075015796,0.0013627944651463349,0.0012838598414108224,0.0011988880778540569,0.0011080154003576363,0.0010113994059361151,0.00090921901978283223,0.00080167438932281373,0.00068898671471105211,0.00057139801539933261,0.0004491708325823507,0.00032258786752443011,0.00019195155596062502,5.7583578959622154e-005,-8.0175689169209924e-005,-0.00022096779514753923,-0.0003644168822067843,-0.00051013042560854366,-0.00065770002684943521,-0.00080670226500325368,-0.00095669960346631607,-0.0011072413501884279,-0.0012578646692921293,-0.0014080956418081206,-0.0015574503730848967,-0.0017054361442666096,-0.0018515526050755034,-0.0019952930049844,-0.0021361454597212258,-0.0022735942499120449,-0.002407121148541704,-0.002536206773792949,-0.002660331963715535,-0.002778979169077554,-0.0028916338606616515,-0.0029977859471897565,-0.0030969311999917899,-0.0031885726804763009,-0.0032722221664152444,-0.0033474015730203888,-0.0034136443647661092,-0.0034704969539024327,-0.0035175200816031514,-0.0035542901777068986,-0.0035804006950342564,-0.0035954634143013733,-0.0035991097156997417,-0.003590991813273341,-0.0035707839482976332,-0.0035381835379500338,-0.0034929122756577917,-0.003434717179618089,-0.0033633715861032597,-0.0032786760842950383,-0.0031804593895316713,-0.0030685791520025825,-0.0029429226980858219,-0.0028034077016931341,-0.0026499827831662514,-0.0024826280334550822,-0.0023013554615033756,-0.0021062093629695927,-0.001897266608620157,-0.0016746368509468895,-0.0014384626477818459,-0.0011889195019081484,-0.00092621581589546922,-0.00065059276162264122,-0.00036232406418628288,-6.1715700133161395e-005,0.0002508944898054109,0.0005751372730580461,0.00091061258197575555,0.0012568901541113145,0.0016135102548747427,0.0019799844811869845,0.0023557966445193704,0.0027404037314752707,0.0031332369398437421,0.0035337027878328056,0.0039411842939737745,0.0043550422249778366,0.0047746164086229156,0.0051992271085530249,0.0056281764576849112,0.0060607499467376534,0.0064962179642317095,0.0069338373841437402,0.0073728531972545187,0.007812500182088223,0.0082520046112148152,0.0086905859885705247,0.0091274588133489921,0.0095618343659236299,0.0099929225111838622,0.010419933514602096,0.01084207986629613,0.011258578108313344,0.011668650660336679,0.012071527639002218,0.012466448666019104,0.012852664660299374,0.013229439609334401,0.013596052315098202,0.013951798109815301,0.014295990537000303,0.014627962993261551,0.014947070326456138,0.015252690385895273,0.015544225520419553,0.015821104020298644,0.016082781499055957,0.016328742211476139,0.016558500304221256,0.016771600995662259,0.016967621681717546,0.017146172964692522,0.017306899602317884,0.017449481374401703,0.017573633864731805,0.017679109156094979,0.017765696436516064,0.017833222515060979,0.01788155224579371,0.017910588858729381,0.017920274196877826,0.017910588858729381,0.01788155224579371,0.017833222515060979,0.017765696436516064,0.017679109156094979,0.017573633864731805,0.017449481374401703,0.017306899602317884,0.017146172964692522,0.016967621681717546,0.016771600995662259,0.016558500304221256,0.016328742211476139,0.016082781499055957,0.015821104020298644,0.015544225520419553,0.015252690385895273,0.014947070326456138,0.014627962993261551,0.014295990537000303,0.013951798109815301,0.013596052315098202,0.013229439609334401,0.012852664660299374,0.012466448666019104,0.012071527639002218,0.011668650660336679,0.011258578108313344,0.01084207986629613,0.010419933514602096,0.0099929225111838622,0.0095618343659236299,0.0091274588133489921,0.0086905859885705247,0.0082520046112148152,0.007812500182088223,0.0073728531972545187,0.0069338373841437402,0.0064962179642317095,0.0060607499467376534,0.0056281764576849112,0.0051992271085530249,0.0047746164086229156,0.0043550422249778366,0.0039411842939737745,0.0035337027878328056,0.0031332369398437421,0.0027404037314752707,0.0023557966445193704,0.0019799844811869845,0.0016135102548747427,0.0012568901541113145,0.00091061258197575555,0.0005751372730580461,0.0002508944898054109,-6.1715700133161395e-005,-0.00036232406418628288,-0.00065059276162264122,-0.00092621581589546922,-0.0011889195019081484,-0.0014384626477818459,-0.0016746368509468895,-0.001897266608620157,-0.0021062093629695927,-0.0023013554615033756,-0.0024826280334550822,-0.0026499827831662514,-0.0028034077016931341,-0.0029429226980858219,-0.0030685791520025825,-0.0031804593895316713,-0.0032786760842950383,-0.0033633715861032597,-0.003434717179618089,-0.0034929122756577917,-0.0035381835379500338,-0.0035707839482976332,-0.003590991813273341,-0.0035991097156997417,-0.0035954634143013733,-0.0035804006950342564,-0.0035542901777068986,-0.0035175200816031514,-0.0034704969539024327,-0.0034136443647661092,-0.0033474015730203888,-0.0032722221664152444,-0.0031885726804763009,-0.0030969311999917899,-0.0029977859471897565,-0.0028916338606616515,-0.002778979169077554,-0.002660331963715535,-0.002536206773792949,-0.002407121148541704,-0.0022735942499120449,-0.0021361454597212258,-0.0019952930049844,-0.0018515526050755034,-0.0017054361442666096,-0.0015574503730848967,-0.0014080956418081206,-0.0012578646692921293,-0.0011072413501884279,-0.00095669960346631607,-0.00080670226500325368,-0.00065770002684943521,-0.00051013042560854366,-0.0003644168822067843,-0.00022096779514753923,-8.0175689169209924e-005,5.7583578959622154e-005,0.00019195155596062502,0.00032258786752443011,0.0004491708325823507,0.00057139801539933261,0.00068898671471105211,0.00080167438932281373,0.00090921901978283223,0.0010113994059361151,0.0011080154003576363,0.0011988880778540569,0.0012838598414108224,0.0013627944651463349,0.001435577075015796,0.0015021140681841872,0.0015623329721597911,0.0016161822449466874,0.0016636310176356666,0.0017046687810081695,0.0017393050178763109,0.0017675687830236559,0.0017895082327456444,0.0018051901061150568,0.0018146991602167695,0.00181813756170604,0.0018156242371466917,0.0018072941846783406,0.0017932977496462304,0.0017737998669017623,0.0017489792725479184,0.0017190276879598323,0.0016841489789577672,0.0016445582930469849,0.0016004811776669753,0.0015521526824108444,0.00149981644818455,0.0014437237862751074,0.0013841327502874465,0.0013213072038903552,0.001255515887284487,0.0011870314852684863,0.0011161296997343484,0.0010430883293695205,0.00096818635928169392,0.00089170306319302762,0.00081391712077365577,0.00073510575260060391,0.00065554387513742289,0.00057550327803295657,0.00049525182593448817,0.00041505268690199612,0.00033516358939623833,0.00025583610969489283,0.00017731499146802959,9.9837499117379398e-005,2.3632806353627878e-005,-5.1078578647156268e-005,-0.000124085350302521,-0.00019518590383031607,-0.00026418879367451001,-0.00033091315136185117,-0.00039518906214202178,-0.00045685789990494035,-0.00051577262000956187,-0.0005717980097984704,-0.00062481089671129176,-0.00067470031404693502,-0.00072136762455972435,-0.00076472660220685521,-0.0008047034724942444,-0.00084123691199406738,-0.00087427800772989803,-0.00090379017724398264,-0.00092974905027540754,-0.00095214231308748108,-0.00097096951658728176,-0.00098624184947972517,-0.0009979818777921957,-0.0010062232521940971,-0.0010110103846177473,-0.0010123980957629612,-0.0010104512351375027,-0.0010052442753487613,-0.00099686088241859698,-0.00098539346394335291,-0.00097094269696413064,-0.0009536170374490337,-0.00093353221331832854,-0.00091081070296653683,-0.00088558120125116024,-0.00085797807492699225,-0.00082814080950729702,-0.00079621344952903047,-0.00076234403418854,-0.00072668403029697989,-0.00068938776448143548,-0.00065061185652830786,-0.00061051465573013961,-0.00056925568205609569,-0.00052699507391989206,-0.0004838930442671783,-0.00044010934664787918,-0.00039580275287734901,-0.00035113054382468478,-0.00030624801479634233,-0.00026130799690945073,-0.00021646039577189773,-0.00017185174870559347,-0.00012762480166591899,-8.3918106924121865e-005,-4.0865642491326196e-005,1.4035458276572845e-006,4.2765678954100008e-005,8.3102554925345751e-005,0.0001223018345454641,0.00016025730178152766,0.00019686909848531608,0.00023204393311471031,0.00026569526322344774,0.00029774345158117832,0.00032811589587836045,0.00035674713206145615,0.0003835789114333784,0.00040856025174159277,0.00043164746256151048,0.00045280414536572039,0.00047200116874952889,0.00048921661936057476,0.000504435729154112,0.00051765077966621422,0.00052886098406408188,0.00053807234779593258,0.00054529750872225077,0.00055055555766543823,0.0005538718403659802,0.00055527774188004629,0.00055481045449575904,0.00055251273028330476,0.00054843261942740678,0.00054262319551943007,0.00053514226901057668,0.00052605209004715316,0.00051541904192377883,0.00050331332640081439,0.0004898086421379151,0.00047498185749703369,0.000458912678964864,0.00044168331643729537,0.00042337814659663123,0.00040408337559634581,0.00038388670224828996,0.00036287698288332094,0.00034114389902887882,0.00031877762901577487,0.00029586852459198614,0.00027250679358344331,0.00024878218960101893,0.00022478370974925876,0.00020059930124605364,0.00017631557781390582,0.00015201754665230622,0.00012778834674800928,0.00010370899922518493,7.9858170381307512e-005,5.631194799704007e-005,3.31436314497614e-005,1.0423536100950135e-005,-1.1781187632420663e-005,-3.3406720172927598e-005,-5.439272335515868e-005,-7.468247544720144e-005,-9.4222990453146521e-005,-0.00011296512158832281,-0.00013086364887831731,-0.00014787735089073669,-0.00016396906066624842,-0.00017910570597086024,-0.0001932583340460852,-0.00020640212108628597,-0.00021851636672336324,-0.0002295844738479997,-0.00023959391414360759,-0.00024853617975352944,-0.00025640672154453011,-0.00026320487446929755,-0.00026893377056777621,-0.00027360024018187925,-0.00027721470198962622,-0.00027979104249386351,-0.00028134648562676393,-0.00028190145315427028,-0.00028147941658498082,-0.00028010674130506043,-0.00027781252367493008,-0.00027462842183468704,-0.00027058848097346298,-0.00026572895382303993,-0.00026008811713849791,-0.00025370608492800752,-0.00024662461919051372,-0.00023888693891392331,-0.00023053752807749851,-0.00022162194339074567,-0.00021218662248708597,-0.00020227869327419507,-0.00019194578512417325,-0.00018123584256579615,-0.00017019694211808706,-0.00015887711287947752,-0.00014732416146003336,-0.0001355855018157555,-0.00012370799051387895,-0.00011173776792668028,-9.9720105818573333e-005,-8.7699261757374472e-005,-7.5718340745911544e-005,-6.3819164434437753e-005,-5.2042148238008166e-005,-4.0426186646250175e-005,-2.9008546975628897e-005,-1.7824771777069012e-005,-6.9085900742068874e-006,3.7081624298488197e-006,1.399561407645275e-005,2.3925918842185296e-005,3.3473308768895735e-005,4.2614137241134129e-005,5.1326913154074413e-005,5.959232604923072e-005,6.7393262328148919e-005,7.4714812686558853e-005,8.1544270942065337e-005,8.7871124458095901e-005,9.3687036395020352e-005,9.8985820045867232e-005,0.00010376340553933867,0.00010801779921638294,0.00011174903600823584,0.00011495912516417553,0.00011765198969552485,0.000119833399918863,0.00012151090149635649,0.00012269373838366836,0.00012339277110709677,0.00012362039080059407,0.00012339042944046357,0.00012271806672090166,0.00012161973401708254,0.00012011301588390767,0.00011821654953855146,0.00011594992277288528,0.00011333357073815553,0.00011038867203894197,0.00010713704456644734,0.00010360104149247878,9.980344783558122e-005,9.5767377999136677e-005,9.1516174668330197e-005,8.7073309438981164e-005,8.2462285535489524e-005,7.7706542959057819e-005,7.2829366389609831e-005,6.7853796146420507e-005,6.2802542493354844e-005,5.7697903554303826e-005,5.2561687084014779e-005,4.7415136318033758e-005,4.2278860103952291e-005,3.7172767493934646e-005,3.211600695631732e-005,2.7126910341456481e-005,2.2222941714456671e-005,1.7420651144911701e-005,1.2735633521231706e-005,8.1824924349908408e-006,3.7748091586949831e-006,-4.748832811923351e-007,-4.5551209552641986e-006,-8.4555248461350258e-006,-1.2166811687281863e-005,-1.5680799904973384e-005,-1.8990410761158292e-005,-2.208966481285418e-005,-2.4973673820632981e-005,-2.7638628254697786e-005,-3.0081780562104175e-005,-3.2301424372823877e-005,-3.4296869835144618e-005,-3.6068415283027002e-005,-3.7617315448704588e-005,-3.8945746443507503e-005,-4.0056767738570597e-005,-4.0954281384187789e-005,-4.1642988713078401e-005,-4.2128344777641997e-005,-4.241651077529046e-005,-4.2514304718516722e-005,-4.2429150608084892e-005,-4.2169026367988538e-005,-4.1742410800231418e-005,-4.1158229815698028e-005,-4.0425802194468398e-005,-3.9554785125218651e-005,-3.8555119768387664e-005,-3.7436977081942656e-005,-3.6210704142163814e-005,-3.488677118385603e-005,-3.3475719576492922e-005,-3.1988110943394458e-005',
    'CSET columnconfig = 13',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 22',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 24',
    'CSET decimation_rate = 35',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Decimation',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 2780000',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Chan_ID_Field',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 2',
    'CSET number_paths = 2',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Maximum_Possible',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Chan_ID_Field',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 1',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_966cc03d206c1a0c',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'c3f49e98a9eae4b9e7654fb8143b10df',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_966cc03d206c1a0c
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_s_data_chanid_incorrect:out std_logic;
      m_axis_data_tdata:out std_logic_vector(63 downto 0);
      m_axis_data_tuser:out std_logic_vector(0 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(47 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tuser:in std_logic_vector(0 downto 0);
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(63 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path1_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_path0_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chanid_ps_net: std_logic_vector(0 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(47 downto 0) := (others=>\'0\');
signal s_axis_data_tuser_net: std_logic_vector(0 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_path1_ps_net <= m_axis_data_tdata_net(56 downto 32);
  m_axis_data_tdata_path0_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chanid_ps_net <= m_axis_data_tuser_net(0 downto 0);
  s_axis_data_tdata_net(47 downto 24) <= s_axis_data_tdata_path1;
  s_axis_data_tdata_net(23 downto 0) <= s_axis_data_tdata_path0;
  s_axis_data_tuser_net(0 downto 0) <= s_axis_data_tuser_chanid;
  m_axis_data_tdata_path1_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path1_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tdata_path1
    );
  m_axis_data_tdata_path0_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_path0_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tdata_path0
    );
  m_axis_data_tuser_chanid_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chanid_ps_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o => m_axis_data_tuser_chanid
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_35,
        clr => \'0\',
        clk => clk_35, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_35, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_966cc03d206c1a0c_instance : fr_cmplr_v6_3_966cc03d206c1a0c
    port map(
      aclk=>clk,
      aclken=>ce,
      event_s_data_chanid_incorrect=>event_s_data_chanid_incorrect,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tuser=>s_axis_data_tuser_net,
      s_axis_data_tvalid=>ce_logic_1
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_35:in std_logic;
    ce_logic_1:in std_logic;
    clk:in std_logic;
    clk_35:in std_logic;
    clk_logic_1:in std_logic;
    event_s_data_chanid_incorrect:out std_logic;
    m_axis_data_tdata_path0:out std_logic_vector(24 downto 0);
    m_axis_data_tdata_path1:out std_logic_vector(24 downto 0);
    m_axis_data_tuser_chanid:out std_logic_vector(0 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata_path0:in std_logic_vector(23 downto 0);
    s_axis_data_tdata_path1:in std_logic_vector(23 downto 0);
    s_axis_data_tready:out std_logic;
    s_axis_data_tuser_chanid:in std_logic_vector(0 downto 0);
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_7b42571465e0fc6f89e7bb14ed540bde',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'a0577be9ed2da765ba2d0429c9a5eafa',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'bf5eb78e468802fb5a2357f890bd2095',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal sel_1_20: std_logic_vector((2 - 1) downto 0);
  signal d0_1_24: std_logic_vector((24 - 1) downto 0);
  signal d1_1_27: std_logic_vector((24 - 1) downto 0);
  signal d2_1_30: std_logic_vector((24 - 1) downto 0);
  signal d3_1_33: std_logic_vector((24 - 1) downto 0);
  signal unregy_join_6_1: std_logic_vector((24 - 1) downto 0);
begin
  sel_1_20 <= sel;
  d0_1_24 <= d0;
  d1_1_27 <= d1;
  d2_1_30 <= d2;
  d3_1_33 <= d3;
  proc_switch_6_1: process (d0_1_24, d1_1_27, d2_1_30, d3_1_33, sel_1_20)
  is
  begin
    case sel_1_20 is 
      when "00" =>
        unregy_join_6_1 <= d0_1_24;
      when "01" =>
        unregy_join_6_1 <= d1_1_27;
      when "10" =>
        unregy_join_6_1 <= d2_1_30;
      when others =>
        unregy_join_6_1 <= d3_1_33;
    end case;
  end process proc_switch_6_1;
  y <= unregy_join_6_1;
end',
      'crippled_entity' => 'is
  port (
    sel : in std_logic_vector((2 - 1) downto 0);
    d0 : in std_logic_vector((24 - 1) downto 0);
    d1 : in std_logic_vector((24 - 1) downto 0);
    d2 : in std_logic_vector((24 - 1) downto 0);
    d3 : in std_logic_vector((24 - 1) downto 0);
    y : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'mux_f062741975',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT Binary_Counter virtex6 Xilinx,_Inc. 11.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET ainit_value = 0',
    'CSET ce = true',
    'CSET count_mode = UP',
    'CSET fb_latency = 0',
    'CSET final_count_value = 1',
    'CSET implementation = Fabric',
    'CSET increment_value = 1',
    'CSET latency = 1',
    'CSET load = false',
    'CSET output_width = 2',
    'CSET restrict_count = false',
    'CSET sclr = false',
    'CSET sinit = true',
    'CSET sinit_value = 0',
    'CSET sset = false',
    'CSET sync_ce_priority = Sync_Overrides_CE',
    'CSET sync_threshold_output = false',
    'CSET syncctrlpriority = Reset_Overrides_Set',
    'CSET component_name = cntr_11_0_3166d4cc5b09c744',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'be0aae355dfb7886fc8a19dd53b74d1a',
    'sourceFile' => 'hdl/xlcounter_free.vhd',
    'templateKeyValues' => {
      'core_component_def' => '      clk: in std_logic;
      ce: in std_logic;
      SINIT: in std_logic;
      q: out std_logic_vector(op_width - 1 downto 0)',
      'core_instance_text' => '        clk => clk,
        ce => core_ce,
        SINIT => core_sinit,
        q => op_net',
      'core_name0' => 'cntr_11_0_3166d4cc5b09c744',
      'entity_name.0' => 'xlcounter_free',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '12d2614be27c6300414d93a00904e5fc',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '770d2f496fcabeae7299a45a8251944c',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '504fadefd5d1003b60362f261f5e06cf',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlceprobe.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '048c6a870c7b0c041670c5d42ccfc26c',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal sel_1_20: std_logic_vector((2 - 1) downto 0);
  signal d0_1_24: std_logic_vector((25 - 1) downto 0);
  signal d1_1_27: std_logic_vector((25 - 1) downto 0);
  signal d2_1_30: std_logic_vector((25 - 1) downto 0);
  signal d3_1_33: std_logic_vector((25 - 1) downto 0);
  signal unregy_join_6_1: std_logic_vector((25 - 1) downto 0);
begin
  sel_1_20 <= sel;
  d0_1_24 <= d0;
  d1_1_27 <= d1;
  d2_1_30 <= d2;
  d3_1_33 <= d3;
  proc_switch_6_1: process (d0_1_24, d1_1_27, d2_1_30, d3_1_33, sel_1_20)
  is
  begin
    case sel_1_20 is 
      when "00" =>
        unregy_join_6_1 <= d0_1_24;
      when "01" =>
        unregy_join_6_1 <= d1_1_27;
      when "10" =>
        unregy_join_6_1 <= d2_1_30;
      when others =>
        unregy_join_6_1 <= d3_1_33;
    end case;
  end process proc_switch_6_1;
  y <= unregy_join_6_1;
end',
      'crippled_entity' => 'is
  port (
    sel : in std_logic_vector((2 - 1) downto 0);
    d0 : in std_logic_vector((25 - 1) downto 0);
    d1 : in std_logic_vector((25 - 1) downto 0);
    d2 : in std_logic_vector((25 - 1) downto 0);
    d3 : in std_logic_vector((25 - 1) downto 0);
    y : out std_logic_vector((25 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'mux_8380cdcd3c',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '3a9b696411cf2067ced21739645435c7',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlusamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b32a0080f8f47e0be7ec44c6ad81b20b',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal ip_1_26: boolean;
  type array_type_op_mem_22_20 is array (0 to (1 - 1)) of boolean;
  signal op_mem_22_20: array_type_op_mem_22_20 := (
    0 => false);
  signal op_mem_22_20_front_din: boolean;
  signal op_mem_22_20_back: boolean;
  signal op_mem_22_20_push_front_pop_back_en: std_logic;
  signal internal_ip_12_1_bitnot: boolean;
begin
  ip_1_26 <= ((ip) = "1");
  op_mem_22_20_back <= op_mem_22_20(0);
  proc_op_mem_22_20: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_22_20_push_front_pop_back_en = \'1\')) then
        op_mem_22_20(0) <= op_mem_22_20_front_din;
      end if;
    end if;
  end process proc_op_mem_22_20;
  internal_ip_12_1_bitnot <= ((not boolean_to_vector(ip_1_26)) = "1");
  op_mem_22_20_push_front_pop_back_en <= \'0\';
  op <= boolean_to_vector(internal_ip_12_1_bitnot);
end',
      'crippled_entity' => 'is
  port (
    ip : in std_logic_vector((1 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'inverter_e5b38cca3b',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '298203483c3de52896eed04fd75246a4',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal d0_1_24: std_logic;
  signal d1_1_27: std_logic;
  signal fully_2_1_bit: std_logic;
begin
  d0_1_24 <= d0(0);
  d1_1_27 <= d1(0);
  fully_2_1_bit <= d0_1_24 and d1_1_27;
  y <= std_logic_to_vector(fully_2_1_bit);
end',
      'crippled_entity' => 'is
  port (
    d0 : in std_logic_vector((1 - 1) downto 0);
    d1 : in std_logic_vector((1 - 1) downto 0);
    y : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'logical_80f90b97d0',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '029148a9119c4a8bdc326876071b425f',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal in0_1_23: unsigned((26 - 1) downto 0);
  signal in1_1_27: unsigned((24 - 1) downto 0);
  signal y_2_1_concat: unsigned((50 - 1) downto 0);
begin
  in0_1_23 <= std_logic_vector_to_unsigned(in0);
  in1_1_27 <= std_logic_vector_to_unsigned(in1);
  y_2_1_concat <= std_logic_vector_to_unsigned(unsigned_to_std_logic_vector(in0_1_23) & unsigned_to_std_logic_vector(in1_1_27));
  y <= unsigned_to_std_logic_vector(y_2_1_concat);
end',
      'crippled_entity' => 'is
  port (
    in0 : in std_logic_vector((26 - 1) downto 0);
    in1 : in std_logic_vector((24 - 1) downto 0);
    y : out std_logic_vector((50 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'concat_0f2aa0366d',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'a406efbaa0a60b152f8dd98fde737e08',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlconvert.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'c64d42e35ae4b2c6e596525e6220461a',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: unsigned((50 - 1) downto 0);
  signal output_port_5_5_force: signed((50 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_unsigned(input_port);
  output_port_5_5_force <= unsigned_to_signed(input_port_1_40);
  output_port <= signed_to_std_logic_vector(output_port_5_5_force);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((50 - 1) downto 0);
    output_port : out std_logic_vector((50 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_1d284b35d5',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '3d80ef3d90af7493c76f4972f4ceedc2',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: unsigned((24 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_unsigned(input_port);
  output_port <= unsigned_to_std_logic_vector(input_port_1_40);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((24 - 1) downto 0);
    output_port : out std_logic_vector((24 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_950635102d',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '412bdb084609c6dc85ea3856e400c1cc',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal input_port_1_40: signed((26 - 1) downto 0);
  signal output_port_5_5_force: unsigned((26 - 1) downto 0);
begin
  input_port_1_40 <= std_logic_vector_to_signed(input_port);
  output_port_5_5_force <= signed_to_unsigned(input_port_1_40);
  output_port <= unsigned_to_std_logic_vector(output_port_5_5_force);
end',
      'crippled_entity' => 'is
  port (
    input_port : in std_logic_vector((26 - 1) downto 0);
    output_port : out std_logic_vector((26 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'reinterpret_f5c4fbeb84',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => '318de55d4765969d57088b2e278af43a',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlpassthrough.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '86c87ac01e65200b9c3878185fcf10e8',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlpassthrough.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'e9441fff49b79fa88003c6783e538c40',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlconvert.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '8c1e9659ff107910952fe919a066369f',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldelay.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'd717f16731349d8a0d607c8291a9509a',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'c90f8b9670a4f92c5a98b48551d16d23',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b9b5a0654ab7a1506bb84d7abb81b712',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '99294b4642e58229309b91a24a0ad3eb',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_24: boolean;
  signal b_1_27: boolean;
  signal c_1_30: boolean;
  signal bit_6_36: boolean;
  signal fulldout_6_2_bit: boolean;
begin
  a_1_24 <= ((a) = "1");
  b_1_27 <= ((b) = "1");
  c_1_30 <= ((c) = "1");
  bit_6_36 <= ((boolean_to_vector(b_1_27) and boolean_to_vector(a_1_24)) = "1");
  fulldout_6_2_bit <= ((boolean_to_vector(c_1_30) and boolean_to_vector(bit_6_36)) = "1");
  dout <= boolean_to_vector(fulldout_6_2_bit);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((1 - 1) downto 0);
    b : in std_logic_vector((1 - 1) downto 0);
    c : in std_logic_vector((1 - 1) downto 0);
    dout : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'expr_375d7bbece',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT Divider_Generator virtex6 Xilinx,_Inc. 4.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET aclken = true',
    'CSET algorithm_type = High_Radix',
    'CSET aresetn = false',
    'CSET clocks_per_division = 1',
    'CSET divide_by_zero_detect = false',
    'CSET dividend_and_quotient_width = 26',
    'CSET dividend_has_tlast = false',
    'CSET dividend_has_tuser = false',
    'CSET dividend_tuser_width = 1',
    'CSET divisor_has_tlast = false',
    'CSET divisor_has_tuser = false',
    'CSET divisor_tuser_width = 1',
    'CSET divisor_width = 26',
    'CSET flowcontrol = NonBlocking',
    'CSET fractional_width = 46',
    'CSET latency = 25',
    'CSET latency_configuration = Manual',
    'CSET operand_sign = Signed',
    'CSET optimizegoal = Performance',
    'CSET outtlastbehv = Null',
    'CSET outtready = false',
    'CSET remainder_type = Fractional',
    'CSET component_name = dv_gn_v4_0_7887f22978e0003d',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '1bb6c92d035537b3f23bf704ee2b7a8d',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component dv_gn_v4_0_7887f22978e0003d
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_dout_tdata:out std_logic_vector(71 downto 0);
      m_axis_dout_tvalid:out std_logic;
      s_axis_dividend_tdata:in std_logic_vector(31 downto 0);
      s_axis_dividend_tready:out std_logic;
      s_axis_dividend_tvalid:in std_logic;
      s_axis_divisor_tdata:in std_logic_vector(31 downto 0);
      s_axis_divisor_tready:out std_logic;
      s_axis_divisor_tvalid:in std_logic
    );
end component;
signal m_axis_dout_tdata_net: std_logic_vector(71 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_quotient_net: std_logic_vector(25 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_shift_out_net: std_logic_vector(49 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_fractional_net: std_logic_vector(45 downto 0) := (others=>\'0\');
signal s_axis_dividend_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal s_axis_divisor_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
begin
  m_axis_dout_tdata_quotient_net <= m_axis_dout_tdata_net(71 downto 46);
  m_axis_dout_tdata_fractional_net <= m_axis_dout_tdata_net(45 downto 0);
  s_axis_dividend_tdata_net(25 downto 0) <= s_axis_dividend_tdata_dividend;
  s_axis_divisor_tdata_net(25 downto 0) <= s_axis_divisor_tdata_divisor;
  m_axis_dout_tdata_quotient <= m_axis_dout_tdata_shift_out_net(49 downto 24);
m_axis_dout_tdata_fractional <= m_axis_dout_tdata_shift_out_net(23 downto 0);
m_axis_dout_tdata_shift_out_net <= shift_division_result(m_axis_dout_tdata_quotient_net, m_axis_dout_tdata_fractional_net, 24, 0, 1);
  dv_gn_v4_0_7887f22978e0003d_instance : dv_gn_v4_0_7887f22978e0003d
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_dout_tdata=>m_axis_dout_tdata_net,
      m_axis_dout_tvalid=>m_axis_dout_tvalid,
      s_axis_dividend_tdata=>s_axis_dividend_tdata_net,
      s_axis_dividend_tready=>s_axis_dividend_tready,
      s_axis_dividend_tvalid=>s_axis_dividend_tvalid,
      s_axis_divisor_tdata=>s_axis_divisor_tdata_net,
      s_axis_divisor_tready=>s_axis_divisor_tready,
      s_axis_divisor_tvalid=>s_axis_divisor_tvalid
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    clk:in std_logic;
    m_axis_dout_tdata_fractional:out std_logic_vector(23 downto 0);
    m_axis_dout_tdata_quotient:out std_logic_vector(25 downto 0);
    m_axis_dout_tvalid:out std_logic;
    s_axis_dividend_tdata_dividend:in std_logic_vector(25 downto 0);
    s_axis_dividend_tready:out std_logic;
    s_axis_dividend_tvalid:in std_logic;
    s_axis_divisor_tdata_divisor:in std_logic_vector(25 downto 0);
    s_axis_divisor_tready:out std_logic;
    s_axis_divisor_tvalid:in std_logic
  );
end',
      'entity_name' => 'xldivider_generator_7b92d2b3a8529d55359a27d09fb2c5d6',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ac967577b3149d1abb03f10239c790ae',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => 'is
  signal a_1_31: signed((26 - 1) downto 0);
  signal b_1_34: signed((26 - 1) downto 0);
  type array_type_op_mem_32_22 is array (0 to (1 - 1)) of boolean;
  signal op_mem_32_22: array_type_op_mem_32_22 := (
    0 => false);
  signal op_mem_32_22_front_din: boolean;
  signal op_mem_32_22_back: boolean;
  signal op_mem_32_22_push_front_pop_back_en: std_logic;
  signal result_18_3_rel: boolean;
begin
  a_1_31 <= std_logic_vector_to_signed(a);
  b_1_34 <= std_logic_vector_to_signed(b);
  op_mem_32_22_back <= op_mem_32_22(0);
  proc_op_mem_32_22: process (clk)
  is
    variable i: integer;
  begin
    if (clk\'event and (clk = \'1\')) then
      if ((ce = \'1\') and (op_mem_32_22_push_front_pop_back_en = \'1\')) then
        op_mem_32_22(0) <= op_mem_32_22_front_din;
      end if;
    end if;
  end process proc_op_mem_32_22;
  result_18_3_rel <= a_1_31 > b_1_34;
  op_mem_32_22_front_din <= result_18_3_rel;
  op_mem_32_22_push_front_pop_back_en <= \'1\';
  op <= boolean_to_vector(op_mem_32_22_back);
end',
      'crippled_entity' => 'is
  port (
    a : in std_logic_vector((26 - 1) downto 0);
    b : in std_logic_vector((26 - 1) downto 0);
    op : out std_logic_vector((1 - 1) downto 0);
    clk : in std_logic;
    ce : in std_logic;
    clr : in std_logic);
end',
      'entity_name' => 'relational_416cfcae1e',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT Divider_Generator virtex6 Xilinx,_Inc. 4.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET aclken = true',
    'CSET algorithm_type = High_Radix',
    'CSET aresetn = false',
    'CSET clocks_per_division = 1',
    'CSET divide_by_zero_detect = false',
    'CSET dividend_and_quotient_width = 26',
    'CSET dividend_has_tlast = false',
    'CSET dividend_has_tuser = false',
    'CSET dividend_tuser_width = 1',
    'CSET divisor_has_tlast = false',
    'CSET divisor_has_tuser = false',
    'CSET divisor_tuser_width = 1',
    'CSET divisor_width = 26',
    'CSET flowcontrol = NonBlocking',
    'CSET fractional_width = 24',
    'CSET latency = 25',
    'CSET latency_configuration = Manual',
    'CSET operand_sign = Signed',
    'CSET optimizegoal = Performance',
    'CSET outtlastbehv = Null',
    'CSET outtready = false',
    'CSET remainder_type = Fractional',
    'CSET component_name = dv_gn_v4_0_fc1ee041a30564f8',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b1aac1b282dfc603e628ba238e20e023',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component dv_gn_v4_0_fc1ee041a30564f8
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      m_axis_dout_tdata:out std_logic_vector(55 downto 0);
      m_axis_dout_tvalid:out std_logic;
      s_axis_dividend_tdata:in std_logic_vector(31 downto 0);
      s_axis_dividend_tready:out std_logic;
      s_axis_dividend_tvalid:in std_logic;
      s_axis_divisor_tdata:in std_logic_vector(31 downto 0);
      s_axis_divisor_tready:out std_logic;
      s_axis_divisor_tvalid:in std_logic
    );
end component;
signal m_axis_dout_tdata_net: std_logic_vector(55 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_quotient_net: std_logic_vector(25 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_shift_out_net: std_logic_vector(49 downto 0) := (others=>\'0\');
signal m_axis_dout_tdata_fractional_net: std_logic_vector(23 downto 0) := (others=>\'0\');
signal s_axis_dividend_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal s_axis_divisor_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
begin
  m_axis_dout_tdata_quotient_net <= m_axis_dout_tdata_net(49 downto 24);
  m_axis_dout_tdata_fractional_net <= m_axis_dout_tdata_net(23 downto 0);
  s_axis_dividend_tdata_net(25 downto 0) <= s_axis_dividend_tdata_dividend;
  s_axis_divisor_tdata_net(25 downto 0) <= s_axis_divisor_tdata_divisor;
  m_axis_dout_tdata_quotient <= m_axis_dout_tdata_shift_out_net(49 downto 24);
m_axis_dout_tdata_fractional <= m_axis_dout_tdata_shift_out_net(23 downto 0);
m_axis_dout_tdata_shift_out_net <= shift_division_result(m_axis_dout_tdata_quotient_net, m_axis_dout_tdata_fractional_net, 24, 0, 0);
  dv_gn_v4_0_fc1ee041a30564f8_instance : dv_gn_v4_0_fc1ee041a30564f8
    port map(
      aclk=>clk,
      aclken=>ce,
      m_axis_dout_tdata=>m_axis_dout_tdata_net,
      m_axis_dout_tvalid=>m_axis_dout_tvalid,
      s_axis_dividend_tdata=>s_axis_dividend_tdata_net,
      s_axis_dividend_tready=>s_axis_dividend_tready,
      s_axis_dividend_tvalid=>s_axis_dividend_tvalid,
      s_axis_divisor_tdata=>s_axis_divisor_tdata_net,
      s_axis_divisor_tready=>s_axis_divisor_tready,
      s_axis_divisor_tvalid=>s_axis_divisor_tvalid
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    clk:in std_logic;
    m_axis_dout_tdata_fractional:out std_logic_vector(23 downto 0);
    m_axis_dout_tdata_quotient:out std_logic_vector(25 downto 0);
    m_axis_dout_tvalid:out std_logic;
    s_axis_dividend_tdata_dividend:in std_logic_vector(25 downto 0);
    s_axis_dividend_tready:out std_logic;
    s_axis_dividend_tvalid:in std_logic;
    s_axis_divisor_tdata_divisor:in std_logic_vector(25 downto 0);
    s_axis_divisor_tready:out std_logic;
    s_axis_divisor_tvalid:in std_logic
  );
end',
      'entity_name' => 'xldivider_generator_9bc6b5677974555f6235feeceb7f929f',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = [
    'SELECT Adder_Subtracter virtex6 Xilinx,_Inc. 11.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET AINIT_Value = 0',
    'CSET A_Type = Signed',
    'CSET A_Width = 25',
    'CSET Add_Mode = Add',
    'CSET B_Constant = false',
    'CSET B_Type = Signed',
    'CSET B_Value = 0',
    'CSET B_Width = 25',
    'CSET Borrow_Sense = Active_Low',
    'CSET Bypass = false',
    'CSET Bypass_CE_Priority = Bypass_Overrides_CE',
    'CSET Bypass_Sense = Active_Low',
    'CSET CE = false',
    'CSET C_In = false',
    'CSET C_Out = false',
    'CSET Implementation = Fabric',
    'CSET Latency = 0',
    'CSET Out_Width = 25',
    'CSET SCLR = false',
    'CSET SINIT = false',
    'CSET SINIT_Value = 0',
    'CSET SSET = false',
    'CSET Sync_CE_Priority = Sync_Overrides_CE',
    'CSET Sync_Ctrl_Priority = Reset_Overrides_Set',
    'CSET component_name = addsb_11_0_26986301a9f671cd',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ee6e25ec6ad3cb966541991f18b127d6',
    'sourceFile' => 'hdl/xladdsub.vhd',
    'templateKeyValues' => {
      'core_component_def' => '    a: in std_logic_vector(25 - 1 downto 0);
    s: out std_logic_vector(c_output_width - 1 downto 0);
    b: in std_logic_vector(25 - 1 downto 0)',
      'core_instance_text' => '         a => full_a,
         s => core_s,
         b => full_b',
      'core_name0' => 'addsb_11_0_26986301a9f671cd',
      'entity_name.0' => 'xladdsub',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Adder_Subtracter virtex6 Xilinx,_Inc. 11.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET AINIT_Value = 0',
    'CSET A_Type = Signed',
    'CSET A_Width = 26',
    'CSET Add_Mode = Subtract',
    'CSET B_Constant = false',
    'CSET B_Type = Signed',
    'CSET B_Value = 0',
    'CSET B_Width = 26',
    'CSET Borrow_Sense = Active_Low',
    'CSET Bypass = false',
    'CSET Bypass_CE_Priority = Bypass_Overrides_CE',
    'CSET Bypass_Sense = Active_Low',
    'CSET CE = false',
    'CSET C_In = false',
    'CSET C_Out = false',
    'CSET Implementation = Fabric',
    'CSET Latency = 0',
    'CSET Out_Width = 26',
    'CSET SCLR = false',
    'CSET SINIT = false',
    'CSET SINIT_Value = 0',
    'CSET SSET = false',
    'CSET Sync_CE_Priority = Sync_Overrides_CE',
    'CSET Sync_Ctrl_Priority = Reset_Overrides_Set',
    'CSET component_name = addsb_11_0_8b0747970e52f130',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '119ac5d050c51df5902b36ede729a751',
    'sourceFile' => 'hdl/xladdsub.vhd',
    'templateKeyValues' => {
      'core_component_def' => '    a: in std_logic_vector(26 - 1 downto 0);
    s: out std_logic_vector(c_output_width - 1 downto 0);
    b: in std_logic_vector(26 - 1 downto 0)',
      'core_instance_text' => '         a => full_a,
         s => core_s,
         b => full_b',
      'core_name0' => 'addsb_11_0_8b0747970e52f130',
      'entity_name.0' => 'xladdsub',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT Adder_Subtracter virtex6 Xilinx,_Inc. 11.0',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET AINIT_Value = 0',
    'CSET A_Type = Signed',
    'CSET A_Width = 26',
    'CSET Add_Mode = Add',
    'CSET B_Constant = false',
    'CSET B_Type = Signed',
    'CSET B_Value = 0',
    'CSET B_Width = 26',
    'CSET Borrow_Sense = Active_Low',
    'CSET Bypass = false',
    'CSET Bypass_CE_Priority = Bypass_Overrides_CE',
    'CSET Bypass_Sense = Active_Low',
    'CSET CE = false',
    'CSET C_In = false',
    'CSET C_Out = false',
    'CSET Implementation = Fabric',
    'CSET Latency = 0',
    'CSET Out_Width = 26',
    'CSET SCLR = false',
    'CSET SINIT = false',
    'CSET SINIT_Value = 0',
    'CSET SSET = false',
    'CSET Sync_CE_Priority = Sync_Overrides_CE',
    'CSET Sync_Ctrl_Priority = Reset_Overrides_Set',
    'CSET component_name = addsb_11_0_239e4f614ba09ab1',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '64b14e7c3f2a998bb43f653f1129b381',
    'sourceFile' => 'hdl/xladdsub.vhd',
    'templateKeyValues' => {
      'core_component_def' => '    a: in std_logic_vector(26 - 1 downto 0);
    s: out std_logic_vector(c_output_width - 1 downto 0);
    b: in std_logic_vector(26 - 1 downto 0)',
      'core_instance_text' => '         a => full_a,
         s => core_s,
         b => full_b',
      'core_name0' => 'addsb_11_0_239e4f614ba09ab1',
      'entity_name.0' => 'xladdsub',
      'needs_core' => 1,
    },
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '07b948ce7c8582cf3246c4206edefe7a',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '04e178b0d2ef98365584c3875c92e648',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'd854c133300e4197b43eb23a82166fca',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'aaa430adaa1b6236b581f52b0ff049d4',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'b712a41e5db4c80909ca63e80729022c',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => 'ced5118472cba66901399fef11405078',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '45e4c798a5fc1bcc9118d45788919174',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = {
    'entity_declaration_hash' => '20a739444f861031799332e2aca75718',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xldsamp.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  $instrs = [
    'SELECT FIR_Compiler virtex6 Xilinx,_Inc. 6.3',
    '# 13.4_O.87xd',
    '# DEVICE virtex6',
    '# VHDL',
    'CSET bestprecision = true',
    'CSET channel_sequence = Basic',
    'CSET clock_frequency = 300',
    'CSET coefficient_buffer_type = Automatic',
    'CSET coefficient_file = no_coe_file_loaded',
    'CSET coefficient_fractional_bits = 18',
    'CSET coefficient_reload = false',
    'CSET coefficient_sets = 1',
    'CSET coefficient_sign = Signed',
    'CSET coefficient_structure = Inferred',
    'CSET coefficient_width = 16',
    'CSET coefficientsource = Vector',
    'CSET coefficientvector = 0,-2.8150443134893359e-006,-6.6976238725875895e-006,-1.1304990269963141e-005,-1.604431497857136e-005,-2.0107668908504811e-005,-2.2551548723864894e-005,-2.2417655357545307e-005,-1.8884484009097e-005,-1.14324662712005e-005,0,1.4895245252218591e-005,3.2095008337577838e-005,4.9820225517987918e-005,6.5785115051100965e-005,7.7407541331296344e-005,8.2104253823912598e-005,7.7645572835330541e-005,6.2531307977269543e-005,3.6340120870154232e-005,0,-4.4070606517077606e-005,-9.1992441252299547e-005,-0.00013865131496628788,-0.00017812378842843907,-0.00020428055285240439,-0.00021151874988768109,-0.00019555021117162184,-0.00015415423350615754,-8.7794243989363754e-005,0,0.00010256790594364044,0.0002104267034647511,0.00031197016330681854,0.00039452834212887124,0.00044571081166024763,0.00045491047167882788,0.00041480885983109225,0.00032270247585875563,0.00018146749714553596,0,-0.00020698933341495155,-0.00041988547070270977,-0.00061576751176877582,-0.00077059530835929936,-0.000861802747915486,-0.00087104673941122625,-0.00078681293323371363,-0.0006065607616026848,-0.00033810693938500329,0,0.00037927350285030427,0.00076329437485913372,0.0011108373102628724,0.0013798905245745506,0.0015322143464772445,0.0015379856988276951,0.0013800177155234763,0.0010570380058579536,0.00058556108492878001,0,-0.00064917759672944637,-0.0012992459216626462,-0.0018807504058385085,-0.0023243371190692537,-0.0025682639904175543,-0.0025658408413689869,-0.0022919808744064647,-0.0017480656588507297,-0.00096443416645807867,0,0.0010612340945903454,0.0021166817264924782,0.0030542832592820018,0.0037634891815493277,0.0041471187476899117,0.0041329142567326201,0.00368353961134064,0.0028038268898484147,0.0015442609210198023,0,-0.0016948619659709831,-0.0033776154840431298,-0.0048711705413799714,-0.0060010788068776662,-0.0066138342553605893,-0.0065946878434032449,-0.0058831167309433404,-0.0044841751704463514,-0.0024742265994219981,0,0.0027295047669241785,0.005458067226404714,0.0079033579580103045,0.0097825581636990979,0.010840381318230943,0.010877006554497207,0.0097732642798801939,0.0075104897250429506,0.0041827862769845169,0,-0.0047195413599075689,-0.0095665889624341766,-0.014067960771227194,-0.017721179958304981,-0.020034200470068875,-0.020567066295356224,-0.018972020184954679,-0.015028546890858736,-0.0086701194720131738,0,0.010705715087948866,0.02300858789139747,0.036331525716839785,0.049994187915671576,0.063257609665124753,0.075374703493635523,0.085642735226220831,0.093453633434288683,0.098338106184078192,0.10000000000000001,0.098338106184078192,0.093453633434288683,0.085642735226220831,0.075374703493635523,0.063257609665124753,0.049994187915671576,0.036331525716839785,0.02300858789139747,0.010705715087948866,0,-0.0086701194720131738,-0.015028546890858736,-0.018972020184954679,-0.020567066295356224,-0.020034200470068875,-0.017721179958304981,-0.014067960771227194,-0.0095665889624341766,-0.0047195413599075689,0,0.0041827862769845169,0.0075104897250429506,0.0097732642798801939,0.010877006554497207,0.010840381318230943,0.0097825581636990979,0.0079033579580103045,0.005458067226404714,0.0027295047669241785,0,-0.0024742265994219981,-0.0044841751704463514,-0.0058831167309433404,-0.0065946878434032449,-0.0066138342553605893,-0.0060010788068776662,-0.0048711705413799714,-0.0033776154840431298,-0.0016948619659709831,0,0.0015442609210198023,0.0028038268898484147,0.00368353961134064,0.0041329142567326201,0.0041471187476899117,0.0037634891815493277,0.0030542832592820018,0.0021166817264924782,0.0010612340945903454,0,-0.00096443416645807867,-0.0017480656588507297,-0.0022919808744064647,-0.0025658408413689869,-0.0025682639904175543,-0.0023243371190692537,-0.0018807504058385085,-0.0012992459216626462,-0.00064917759672944637,0,0.00058556108492878001,0.0010570380058579536,0.0013800177155234763,0.0015379856988276951,0.0015322143464772445,0.0013798905245745506,0.0011108373102628724,0.00076329437485913372,0.00037927350285030427,0,-0.00033810693938500329,-0.0006065607616026848,-0.00078681293323371363,-0.00087104673941122625,-0.000861802747915486,-0.00077059530835929936,-0.00061576751176877582,-0.00041988547070270977,-0.00020698933341495155,0,0.00018146749714553596,0.00032270247585875563,0.00041480885983109225,0.00045491047167882788,0.00044571081166024763,0.00039452834212887124,0.00031197016330681854,0.0002104267034647511,0.00010256790594364044,0,-8.7794243989363754e-005,-0.00015415423350615754,-0.00019555021117162184,-0.00021151874988768109,-0.00020428055285240439,-0.00017812378842843907,-0.00013865131496628788,-9.1992441252299547e-005,-4.4070606517077606e-005,0,3.6340120870154232e-005,6.2531307977269543e-005,7.7645572835330541e-005,8.2104253823912598e-005,7.7407541331296344e-005,6.5785115051100965e-005,4.9820225517987918e-005,3.2095008337577838e-005,1.4895245252218591e-005,0,-1.14324662712005e-005,-1.8884484009097e-005,-2.2417655357545307e-005,-2.2551548723864894e-005,-2.0107668908504811e-005,-1.604431497857136e-005,-1.1304990269963141e-005,-6.6976238725875895e-006,-2.8150443134893359e-006',
    'CSET columnconfig = 1',
    'CSET data_buffer_type = Automatic',
    'CSET data_fractional_bits = 24',
    'CSET data_has_tlast = Not_Required',
    'CSET data_sign = Signed',
    'CSET data_tuser_width = 1',
    'CSET data_width = 25',
    'CSET decimation_rate = 10',
    'CSET displayreloadorder = false',
    'CSET filter_architecture = Systolic_Multiply_Accumulate',
    'CSET filter_selection = 1',
    'CSET filter_type = Decimation',
    'CSET gen_mif_files = false',
    'CSET gen_mif_from_coe = false',
    'CSET gen_mif_from_spec = false',
    'CSET gui_behaviour = Sysgen_uUPP',
    'CSET hardwareoversamplingrate = 5560000',
    'CSET has_aclken = true',
    'CSET has_aresetn = false',
    'CSET input_buffer_type = Automatic',
    'CSET inter_column_pipe_length = 4',
    'CSET interpolation_rate = 1',
    'CSET m_data_has_tready = false',
    'CSET m_data_has_tuser = Chan_ID_Field',
    'CSET multi_column_support = Automatic',
    'CSET num_reload_slots = 1',
    'CSET number_channels = 4',
    'CSET number_paths = 1',
    'CSET optimization_goal = Area',
    'CSET output_buffer_type = Automatic',
    'CSET output_rounding_mode = Convergent_Rounding_to_Even',
    'CSET output_width = 25',
    'CSET passband_max = 0.50000000000',
    'CSET passband_min = 0',
    'CSET pattern_list = P4-0,P4-1,P4-2,P4-3,P4-4',
    'CSET preference_for_other_storage = Automatic',
    'CSET quantization = Quantize_Only',
    'CSET rate_change_type = Integer',
    'CSET ratespecification = Hardware_Oversampling_Rate',
    'CSET reload_file = no_coe_file_loaded',
    'CSET reset_data_vector = true',
    'CSET s_config_method = Single',
    'CSET s_config_sync_mode = On_Vector',
    'CSET s_data_has_fifo = false',
    'CSET s_data_has_tuser = Chan_ID_Field',
    'CSET sample_frequency = 0.00100000000',
    'CSET sampleperiod = 5560000',
    'CSET select_pattern = All',
    'CSET stopband_max = 1',
    'CSET stopband_min = 0.50000000000',
    'CSET zero_pack_factor = 1',
    'SET device = xc6vlx240t',
    'SET package = ff1156',
    'SET speedgrade = -1',
    'CSET component_name = fr_cmplr_v6_3_d835bd2112f47abf',
    'GENERATE',
  ];
  push(@$results, &SgGenerateCores::saveXcoSequence($instrs));
  $instrs = {
    'entity_declaration_hash' => '5ab74296af54d14624d3c25e9bf719f0',
    'sourceFile' => 'hdl/xlmcode.vhd',
    'templateKeyValues' => {
      'crippled_architecture' => ' is
  component fr_cmplr_v6_3_d835bd2112f47abf
    port(
      aclk:in std_logic;
      aclken:in std_logic;
      event_s_data_chanid_incorrect:out std_logic;
      m_axis_data_tdata:out std_logic_vector(31 downto 0);
      m_axis_data_tuser:out std_logic_vector(1 downto 0);
      m_axis_data_tvalid:out std_logic;
      s_axis_data_tdata:in std_logic_vector(31 downto 0);
      s_axis_data_tready:out std_logic;
      s_axis_data_tuser:in std_logic_vector(1 downto 0);
      s_axis_data_tvalid:in std_logic
    );
end component;
signal m_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal m_axis_data_tdata_ps_net: std_logic_vector(24 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tuser_chanid_ps_net: std_logic_vector(1 downto 0) := (others=>\'0\');
signal m_axis_data_tvalid_ps_net: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_captured: std_logic := \'0\';
signal m_axis_data_tvalid_ps_net_or_captured_net: std_logic := \'0\';
signal s_axis_data_tdata_net: std_logic_vector(31 downto 0) := (others=>\'0\');
signal s_axis_data_tuser_net: std_logic_vector(1 downto 0) := (others=>\'0\');
begin
  m_axis_data_tdata_ps_net <= m_axis_data_tdata_net(24 downto 0);
  m_axis_data_tuser_chanid_ps_net <= m_axis_data_tuser_net(1 downto 0);
  s_axis_data_tdata_net(24 downto 0) <= s_axis_data_tdata;
  s_axis_data_tuser_net(1 downto 0) <= s_axis_data_tuser_chanid;
  m_axis_data_tdata_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 25,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tdata_ps_net,
        ce => ce_55600000,
        clr => \'0\',
        clk => clk_55600000, 
        o => m_axis_data_tdata
    );
  m_axis_data_tuser_chanid_ps_net_synchronizer : entity work.synth_reg_w_init
    generic map(
        width => 2,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i => m_axis_data_tuser_chanid_ps_net,
        ce => ce_55600000,
        clr => \'0\',
        clk => clk_55600000, 
        o => m_axis_data_tuser_chanid
    );
  m_axis_data_tvalid_ps_net_or_captured_net <= m_axis_data_tvalid_ps_net or m_axis_data_tvalid_ps_net_captured;
m_axis_data_tvalid_ps_net_synchronizer_1 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => m_axis_data_tvalid_ps_net_or_captured_net,
        ce => ce_55600000,
        clr => \'0\',
        clk => clk_55600000, 
        o(0) => m_axis_data_tvalid
    );
m_axis_data_tvalid_ps_net_synchronizer_2 : entity work.synth_reg_w_init
    generic map(
        width => 1,
        init_index => 0,
        init_value => "0",
        latency => 1
    )
    port map (
        i(0) => \'1\',
        ce => m_axis_data_tvalid_ps_net,
        clr => \'0\',
        clk => clk_55600000, 
        o(0) => m_axis_data_tvalid_ps_net_captured
    );
  fr_cmplr_v6_3_d835bd2112f47abf_instance : fr_cmplr_v6_3_d835bd2112f47abf
    port map(
      aclk=>clk,
      aclken=>ce,
      event_s_data_chanid_incorrect=>event_s_data_chanid_incorrect,
      m_axis_data_tdata=>m_axis_data_tdata_net,
      m_axis_data_tuser=>m_axis_data_tuser_net,
      m_axis_data_tvalid=>m_axis_data_tvalid_ps_net,
      s_axis_data_tdata=>s_axis_data_tdata_net,
      s_axis_data_tready=>s_axis_data_tready,
      s_axis_data_tuser=>s_axis_data_tuser_net,
      s_axis_data_tvalid=>ce_logic_5560000
    );
end ',
      'crippled_entity' => 'is 
  port(
    ce:in std_logic;
    ce_5560000:in std_logic;
    ce_55600000:in std_logic;
    ce_logic_5560000:in std_logic;
    clk:in std_logic;
    clk_5560000:in std_logic;
    clk_55600000:in std_logic;
    clk_logic_5560000:in std_logic;
    event_s_data_chanid_incorrect:out std_logic;
    m_axis_data_tdata:out std_logic_vector(24 downto 0);
    m_axis_data_tuser_chanid:out std_logic_vector(1 downto 0);
    m_axis_data_tvalid:out std_logic;
    s_axis_data_tdata:in std_logic_vector(24 downto 0);
    s_axis_data_tready:out std_logic;
    s_axis_data_tuser_chanid:in std_logic_vector(1 downto 0);
    src_ce:in std_logic;
    src_clk:in std_logic
  );
end',
      'entity_name' => 'xlfir_compiler_a5803cfd3adc5cc68ebf21435caf3993',
    },
  };
  push(@$results, &SgDeliverFile::deliverFile($instrs));
  $instrs = {
    'entity_declaration_hash' => 'cdc1e147e9da47c60526d77ed2bc2de4',
    'sourceFile' => 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen/hdl/xlregister.vhd',
  };
  push(@$results, &SgDeliverFile::saveCollaborationInfo($instrs));
  local *wrapup = $Sg::{'wrapup'};
  push(@$results, &Sg::wrapup())   if (defined(&wrapup));
  local *wrapup = $SgDeliverFile::{'wrapup'};
  push(@$results, &SgDeliverFile::wrapup())   if (defined(&wrapup));
  local *wrapup = $SgGenerateCores::{'wrapup'};
  push(@$results, &SgGenerateCores::wrapup())   if (defined(&wrapup));
  use Carp qw(croak);
  $ENV{'SYSGEN'} = 'C:/Xilinx/13.4/ISE_DS/ISE/sysgen';
  open(RESULTS, '> D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200') || 
    croak 'couldn\'t open D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200';
  binmode(RESULTS);
  print RESULTS &Sg::toString($results) . "\n";
  close(RESULTS) || 
    croak 'trouble writing D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200';
};

if ($@) {
  open(RESULTS, '> D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200') || 
    croak 'couldn\'t open D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200';
  binmode(RESULTS);
  print RESULTS $@ . "\n";
  close(RESULTS) || 
    croak 'trouble writing D:/Sysgen_projects/dsp-cores-devel/models/netlist_ddc_bpm_476_066/sysgen/script_results8757782492909322200';
  exit(1);
}

exit(0);
