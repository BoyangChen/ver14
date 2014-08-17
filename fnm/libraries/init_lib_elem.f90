    subroutine initialize_lib_elem                                        
                                                                          
        integer ::  nelem=0, ntri=0, nquad=0, nwedge=0, nbrick=0 &        
        &          ,ncoh2d=0, ncoh3d6=0, ncoh3d8=0, nsub2d=0, nsub3d=0 &  
        &          ,nxquad=0, nxbrick=0                                   
        integer :: i=0                                                    
        nelem=2457            
        allocate(lib_elem(nelem))       
        nxbrick=2457     
        allocate(lib_xbrick(nxbrick))   
        call prepare(lib_xbrick(1),key=1, & 
& nodecnc=[2209,1,358,526,4781,2573,2930,3098,5145, 5146,5147, 5148,5149, 5150,5151, 5152,5153, 5154,5155 & 
& , 5156,5157, 5158,5159, 5160], & 
& edgecnc=[1,2,3,4,5,6,7,8], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1),elname="xbrick",eltype="xbrick",typekey=1) 

        call prepare(lib_xbrick(2),key=2, & 
& nodecnc=[548,2210,2209,538,3120,4782,4781,3110,5161, 5162,5163, 5164,5165, 5166,5167, 5168,5169, 5170 & 
& ,5171, 5172,5173, 5174,5175, 5176], & 
& edgecnc=[9,10,11,12,13,14,15,16], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2),elname="xbrick",eltype="xbrick",typekey=2) 

        call prepare(lib_xbrick(3),key=3, & 
& nodecnc=[512,542,539,501,3084,3114,3111,3073,5177, 5178,5179, 5180,5181, 5182,5183, 5184,5185, 5186 & 
& ,5187, 5188,5189, 5190,5191, 5192], & 
& edgecnc=[17,18,19,20,21,22,23,24], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(3),elname="xbrick",eltype="xbrick",typekey=3) 

        call prepare(lib_xbrick(4),key=4, & 
& nodecnc=[509,361,5,529,3081,2933,2577,3101,5193, 5194,5195, 5196,5197, 5198,5199, 5200,5201, 5202,5203 & 
& , 5204,5205, 5206,5207, 5208], & 
& edgecnc=[25,26,27,28,29,30,31,32], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(4),elname="xbrick",eltype="xbrick",typekey=4) 

        call prepare(lib_xbrick(5),key=5, & 
& nodecnc=[2215,529,501,539,4787,3101,3073,3111,5209, 5210,5211, 5212,5182, 5181,5213, 5214,5215, 5216 & 
& ,5217, 5218,5190, 5189,5219, 5220], & 
& edgecnc=[33,34,19,35,36,37,23,38], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(5),elname="xbrick",eltype="xbrick",typekey=5) 

        call prepare(lib_xbrick(6),key=6, & 
& nodecnc=[511,7,362,523,3083,2579,2934,3095,5221, 5222,5223, 5224,5225, 5226,5227, 5228,5229, 5230,5231 & 
& , 5232,5233, 5234,5235, 5236], & 
& edgecnc=[39,40,41,42,43,44,45,46], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(6),elname="xbrick",eltype="xbrick",typekey=6) 

        call prepare(lib_xbrick(7),key=7, & 
& nodecnc=[7,511,503,8,2579,3083,3075,2580,5222, 5221,5237, 5238,5239, 5240,5241, 5242,5230, 5229,5243 & 
& , 5244,5245, 5246,5247, 5248], & 
& edgecnc=[39,47,48,49,43,50,51,52], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(7),elname="xbrick",eltype="xbrick",typekey=7) 

        call prepare(lib_xbrick(8),key=8, & 
& nodecnc=[536,9,363,533,3108,2581,2935,3105,5249, 5250,5251, 5252,5253, 5254,5255, 5256,5257, 5258,5259 & 
& , 5260,5261, 5262,5263, 5264], & 
& edgecnc=[53,54,55,56,57,58,59,60], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(8),elname="xbrick",eltype="xbrick",typekey=8) 

        call prepare(lib_xbrick(9),key=9, & 
& nodecnc=[555,573,536,533,3127,3145,3108,3105,5265, 5266,5267, 5268,5256, 5255,5269, 5270,5271, 5272 & 
& ,5273, 5274,5264, 5263,5275, 5276], & 
& edgecnc=[61,62,56,63,64,65,60,66], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(9),elname="xbrick",eltype="xbrick",typekey=9) 

        call prepare(lib_xbrick(10),key=10, & 
& nodecnc=[9,536,506,364,2581,3108,3078,2936,5250, 5249,5277, 5278,5279, 5280,5281, 5282,5258, 5257,5283 & 
& , 5284,5285, 5286,5287, 5288], & 
& edgecnc=[53,67,68,69,57,70,71,72], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(10),elname="xbrick",eltype="xbrick",typekey=10) 

        call prepare(lib_xbrick(11),key=11, & 
& nodecnc=[364,506,525,10,2936,3078,3097,2582,5280, 5279,5289, 5290,5291, 5292,5293, 5294,5286, 5285,5295 & 
& , 5296,5297, 5298,5299, 5300], & 
& edgecnc=[68,73,74,75,71,76,77,78], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(11),elname="xbrick",eltype="xbrick",typekey=11) 

        call prepare(lib_xbrick(12),key=12, & 
& nodecnc=[11,534,513,365,2583,3106,3085,2937,5301, 5302,5303, 5304,5305, 5306,5307, 5308,5309, 5310,5311 & 
& , 5312,5313, 5314,5315, 5316], & 
& edgecnc=[79,80,81,82,83,84,85,86], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(12),elname="xbrick",eltype="xbrick",typekey=12) 

        call prepare(lib_xbrick(13),key=13, & 
& nodecnc=[535,366,12,517,3107,2938,2584,3089,5317, 5318,5319, 5320,5321, 5322,5323, 5324,5325, 5326,5327 & 
& , 5328,5329, 5330,5331, 5332], & 
& edgecnc=[87,88,89,90,91,92,93,94], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(13),elname="xbrick",eltype="xbrick",typekey=13) 

        call prepare(lib_xbrick(14),key=14, & 
& nodecnc=[365,513,517,12,2937,3085,3089,2584,5306, 5305,5333, 5334,5322, 5321,5335, 5336,5314, 5313,5337 & 
& , 5338,5330, 5329,5339, 5340], & 
& edgecnc=[81,95,89,96,85,97,93,98], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(14),elname="xbrick",eltype="xbrick",typekey=14) 

        call prepare(lib_xbrick(15),key=15, & 
& nodecnc=[13,530,504,14,2585,3102,3076,2586,5341, 5342,5343, 5344,5345, 5346,5347, 5348,5349, 5350,5351 & 
& , 5352,5353, 5354,5355, 5356], & 
& edgecnc=[99,100,101,102,103,104,105,106], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(15),elname="xbrick",eltype="xbrick",typekey=15) 

        call prepare(lib_xbrick(16),key=16, & 
& nodecnc=[532,15,367,514,3104,2587,2939,3086,5357, 5358,5359, 5360,5361, 5362,5363, 5364,5365, 5366,5367 & 
& , 5368,5369, 5370,5371, 5372], & 
& edgecnc=[107,108,109,110,111,112,113,114], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(16),elname="xbrick",eltype="xbrick",typekey=16) 

        call prepare(lib_xbrick(17),key=17, & 
& nodecnc=[15,532,507,368,2587,3104,3079,2940,5358, 5357,5373, 5374,5375, 5376,5377, 5378,5366, 5365,5379 & 
& , 5380,5381, 5382,5383, 5384], & 
& edgecnc=[107,115,116,117,111,118,119,120], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(17),elname="xbrick",eltype="xbrick",typekey=17) 

        call prepare(lib_xbrick(18),key=18, & 
& nodecnc=[368,507,508,16,2940,3079,3080,2588,5376, 5375,5385, 5386,5387, 5388,5389, 5390,5382, 5381,5391 & 
& , 5392,5393, 5394,5395, 5396], & 
& edgecnc=[116,121,122,123,119,124,125,126], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(18),elname="xbrick",eltype="xbrick",typekey=18) 

        call prepare(lib_xbrick(19),key=19, & 
& nodecnc=[17,520,515,369,2589,3092,3087,2941,5397, 5398,5399, 5400,5401, 5402,5403, 5404,5405, 5406,5407 & 
& , 5408,5409, 5410,5411, 5412], & 
& edgecnc=[127,128,129,130,131,132,133,134], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(19),elname="xbrick",eltype="xbrick",typekey=19) 

        call prepare(lib_xbrick(20),key=20, & 
& nodecnc=[369,515,521,18,2941,3087,3093,2590,5402, 5401,5413, 5414,5415, 5416,5417, 5418,5410, 5409,5419 & 
& , 5420,5421, 5422,5423, 5424], & 
& edgecnc=[129,135,136,137,133,138,139,140], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(20),elname="xbrick",eltype="xbrick",typekey=20) 

        call prepare(lib_xbrick(21),key=21, & 
& nodecnc=[18,521,524,370,2590,3093,3096,2942,5416, 5415,5425, 5426,5427, 5428,5429, 5430,5422, 5421,5431 & 
& , 5432,5433, 5434,5435, 5436], & 
& edgecnc=[136,141,142,143,139,144,145,146], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(21),elname="xbrick",eltype="xbrick",typekey=21) 

        call prepare(lib_xbrick(22),key=22, & 
& nodecnc=[19,531,505,20,2591,3103,3077,2592,5437, 5438,5439, 5440,5441, 5442,5443, 5444,5445, 5446,5447 & 
& , 5448,5449, 5450,5451, 5452], & 
& edgecnc=[147,148,149,150,151,152,153,154], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(22),elname="xbrick",eltype="xbrick",typekey=22) 

        call prepare(lib_xbrick(23),key=23, & 
& nodecnc=[20,505,500,371,2592,3077,3072,2943,5442, 5441,5453, 5454,5455, 5456,5457, 5458,5450, 5449,5459 & 
& , 5460,5461, 5462,5463, 5464], & 
& edgecnc=[149,155,156,157,153,158,159,160], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(23),elname="xbrick",eltype="xbrick",typekey=23) 

        call prepare(lib_xbrick(24),key=24, & 
& nodecnc=[371,500,527,21,2943,3072,3099,2593,5456, 5455,5465, 5466,5467, 5468,5469, 5470,5462, 5461,5471 & 
& , 5472,5473, 5474,5475, 5476], & 
& edgecnc=[156,161,162,163,159,164,165,166], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(24),elname="xbrick",eltype="xbrick",typekey=24) 

        call prepare(lib_xbrick(25),key=25, & 
& nodecnc=[22,522,502,23,2594,3094,3074,2595,5477, 5478,5479, 5480,5481, 5482,5483, 5484,5485, 5486,5487 & 
& , 5488,5489, 5490,5491, 5492], & 
& edgecnc=[167,168,169,170,171,172,173,174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(25),elname="xbrick",eltype="xbrick",typekey=25) 

        call prepare(lib_xbrick(26),key=26, & 
& nodecnc=[23,502,499,373,2595,3074,3071,2945,5482, 5481,5493, 5494,5495, 5496,5497, 5498,5490, 5489,5499 & 
& , 5500,5501, 5502,5503, 5504], & 
& edgecnc=[169,175,176,177,173,178,179,180], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(26),elname="xbrick",eltype="xbrick",typekey=26) 

        call prepare(lib_xbrick(27),key=27, & 
& nodecnc=[569,537,498,499,3141,3109,3070,3071,5505, 5506,5507, 5508,5509, 5510,5511, 5512,5513, 5514 & 
& ,5515, 5516,5517, 5518,5519, 5520], & 
& edgecnc=[181,182,183,184,185,186,187,188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(27),elname="xbrick",eltype="xbrick",typekey=27) 

        call prepare(lib_xbrick(28),key=28, & 
& nodecnc=[689,645,94,614,3261,3217,2666,3186,5521, 5522,5523, 5524,5525, 5526,5527, 5528,5529, 5530,5531 & 
& , 5532,5533, 5534,5535, 5536], & 
& edgecnc=[189,190,191,192,193,194,195,196], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(28),elname="xbrick",eltype="xbrick",typekey=28) 

        call prepare(lib_xbrick(29),key=29, & 
& nodecnc=[1049,996,2412,998,3621,3568,4984,3570,5537, 5538,5539, 5540,5541, 5542,5543, 5544,5545, 5546 & 
& ,5547, 5548,5549, 5550,5551, 5552], & 
& edgecnc=[197,198,199,200,201,202,203,204], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(29),elname="xbrick",eltype="xbrick",typekey=29) 

        call prepare(lib_xbrick(30),key=30, & 
& nodecnc=[1157,1189,1208,1176,3729,3761,3780,3748,5553, 5554,5555, 5556,5557, 5558,5559, 5560,5561, 5562 & 
& ,5563, 5564,5565, 5566,5567, 5568], & 
& edgecnc=[205,206,207,208,209,210,211,212], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(30),elname="xbrick",eltype="xbrick",typekey=30) 

        call prepare(lib_xbrick(31),key=31, & 
& nodecnc=[1495,1494,1659,1775,4067,4066,4231,4347,5569, 5570,5571, 5572,5573, 5574,5575, 5576,5577, 5578 & 
& ,5579, 5580,5581, 5582,5583, 5584], & 
& edgecnc=[213,214,215,216,217,218,219,220], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(31),elname="xbrick",eltype="xbrick",typekey=31) 

        call prepare(lib_xbrick(32),key=32, & 
& nodecnc=[1257,1659,1494,1502,3829,4231,4066,4074,5585, 5586,5572, 5571,5587, 5588,5589, 5590,5591, 5592 & 
& ,5580, 5579,5593, 5594,5595, 5596], & 
& edgecnc=[221,214,222,223,224,218,225,226], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(32),elname="xbrick",eltype="xbrick",typekey=32) 

        call prepare(lib_xbrick(33),key=33, & 
& nodecnc=[1463,469,75,1462,4035,3041,2647,4034,5597, 5598,5599, 5600,5601, 5602,5603, 5604,5605, 5606 & 
& ,5607, 5608,5609, 5610,5611, 5612], & 
& edgecnc=[227,228,229,230,231,232,233,234], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(33),elname="xbrick",eltype="xbrick",typekey=33) 

        call prepare(lib_xbrick(34),key=34, & 
& nodecnc=[469,1463,1496,470,3041,4035,4068,3042,5598, 5597,5613, 5614,5615, 5616,5617, 5618,5606, 5605 & 
& ,5619, 5620,5621, 5622,5623, 5624], & 
& edgecnc=[227,235,236,237,231,238,239,240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(34),elname="xbrick",eltype="xbrick",typekey=34) 

        call prepare(lib_xbrick(35),key=35, & 
& nodecnc=[348,1421,2253,1619,2920,3993,4825,4191,5625, 5626,5627, 5628,5629, 5630,5631, 5632,5633, 5634 & 
& ,5635, 5636,5637, 5638,5639, 5640], & 
& edgecnc=[241,242,243,244,245,246,247,248], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(35),elname="xbrick",eltype="xbrick",typekey=35) 

        call prepare(lib_xbrick(36),key=36, & 
& nodecnc=[1296,1326,1303,1278,3868,3898,3875,3850,5641, 5642,5643, 5644,5645, 5646,5647, 5648,5649, 5650 & 
& ,5651, 5652,5653, 5654,5655, 5656], & 
& edgecnc=[249,250,251,252,253,254,255,256], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(36),elname="xbrick",eltype="xbrick",typekey=36) 

        call prepare(lib_xbrick(37),key=37, & 
& nodecnc=[1310,1324,1326,1296,3882,3896,3898,3868,5657, 5658,5659, 5660,5642, 5641,5661, 5662,5663, 5664 & 
& ,5665, 5666,5650, 5649,5667, 5668], & 
& edgecnc=[257,258,249,259,260,261,253,262], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(37),elname="xbrick",eltype="xbrick",typekey=37) 

        call prepare(lib_xbrick(38),key=38, & 
& nodecnc=[2449,2522,1981,2491,5021,5094,4553,5063,5669, 5670,5671, 5672,5673, 5674,5675, 5676,5677, 5678 & 
& ,5679, 5680,5681, 5682,5683, 5684], & 
& edgecnc=[263,264,265,266,267,268,269,270], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(38),elname="xbrick",eltype="xbrick",typekey=38) 

        call prepare(lib_xbrick(39),key=39, & 
& nodecnc=[1896,281,1929,1987,4468,2853,4501,4559,5685, 5686,5687, 5688,5689, 5690,5691, 5692,5693, 5694 & 
& ,5695, 5696,5697, 5698,5699, 5700], & 
& edgecnc=[271,272,273,274,275,276,277,278], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(39),elname="xbrick",eltype="xbrick",typekey=39) 

        call prepare(lib_xbrick(40),key=40, & 
& nodecnc=[247,1835,1757,1715,2819,4407,4329,4287,5701, 5702,5703, 5704,5705, 5706,5707, 5708,5709, 5710 & 
& ,5711, 5712,5713, 5714,5715, 5716], & 
& edgecnc=[279,280,281,282,283,284,285,286], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(40),elname="xbrick",eltype="xbrick",typekey=40) 

        call prepare(lib_xbrick(41),key=41, & 
& nodecnc=[1516,1555,32,388,4088,4127,2604,2960,5717, 5718,5719, 5720,5721, 5722,5723, 5724,5725, 5726 & 
& ,5727, 5728,5729, 5730,5731, 5732], & 
& edgecnc=[287,288,289,290,291,292,293,294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(41),elname="xbrick",eltype="xbrick",typekey=41) 

        call prepare(lib_xbrick(42),key=42, & 
& nodecnc=[1690,1554,1524,249,4262,4126,4096,2821,5733, 5734,5735, 5736,5737, 5738,5739, 5740,5741, 5742 & 
& ,5743, 5744,5745, 5746,5747, 5748], & 
& edgecnc=[295,296,297,298,299,300,301,302], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(42),elname="xbrick",eltype="xbrick",typekey=42) 

        call prepare(lib_xbrick(43),key=43, & 
& nodecnc=[1692,1553,1552,250,4264,4125,4124,2822,5749, 5750,5751, 5752,5753, 5754,5755, 5756,5757, 5758 & 
& ,5759, 5760,5761, 5762,5763, 5764], & 
& edgecnc=[303,304,305,306,307,308,309,310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(43),elname="xbrick",eltype="xbrick",typekey=43) 

        call prepare(lib_xbrick(44),key=44, & 
& nodecnc=[1838,1780,1726,251,4410,4352,4298,2823,5765, 5766,5767, 5768,5769, 5770,5771, 5772,5773, 5774 & 
& ,5775, 5776,5777, 5778,5779, 5780], & 
& edgecnc=[311,312,313,314,315,316,317,318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(44),elname="xbrick",eltype="xbrick",typekey=44) 

        call prepare(lib_xbrick(45),key=45, & 
& nodecnc=[1823,286,1957,1906,4395,2858,4529,4478,5781, 5782,5783, 5784,5785, 5786,5787, 5788,5789, 5790 & 
& ,5791, 5792,5793, 5794,5795, 5796], & 
& edgecnc=[319,320,321,322,323,324,325,326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(45),elname="xbrick",eltype="xbrick",typekey=45) 

        call prepare(lib_xbrick(46),key=46, & 
& nodecnc=[2085,2042,2118,2142,4657,4614,4690,4714,5797, 5798,5799, 5800,5801, 5802,5803, 5804,5805, 5806 & 
& ,5807, 5808,5809, 5810,5811, 5812], & 
& edgecnc=[327,328,329,330,331,332,333,334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(46),elname="xbrick",eltype="xbrick",typekey=46) 

        call prepare(lib_xbrick(47),key=47, & 
& nodecnc=[1913,1874,2056,1991,4485,4446,4628,4563,5813, 5814,5815, 5816,5817, 5818,5819, 5820,5821, 5822 & 
& ,5823, 5824,5825, 5826,5827, 5828], & 
& edgecnc=[335,336,337,338,339,340,341,342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(47),elname="xbrick",eltype="xbrick",typekey=47) 

        call prepare(lib_xbrick(48),key=48, & 
& nodecnc=[153,2012,2112,2071,2725,4584,4684,4643,5829, 5830,5831, 5832,5833, 5834,5835, 5836,5837, 5838 & 
& ,5839, 5840,5841, 5842,5843, 5844], & 
& edgecnc=[343,344,345,346,347,348,349,350], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(48),elname="xbrick",eltype="xbrick",typekey=48) 

        call prepare(lib_xbrick(49),key=49, & 
& nodecnc=[2017,2034,155,2011,4589,4606,2727,4583,5845, 5846,5847, 5848,5849, 5850,5851, 5852,5853, 5854 & 
& ,5855, 5856,5857, 5858,5859, 5860], & 
& edgecnc=[351,352,353,354,355,356,357,358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(49),elname="xbrick",eltype="xbrick",typekey=49) 

        call prepare(lib_xbrick(50),key=50, & 
& nodecnc=[1839,1781,1697,356,4411,4353,4269,2928,5861, 5862,5863, 5864,5865, 5866,5867, 5868,5869, 5870 & 
& ,5871, 5872,5873, 5874,5875, 5876], & 
& edgecnc=[359,360,361,362,363,364,365,366], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(50),elname="xbrick",eltype="xbrick",typekey=50) 

        call prepare(lib_xbrick(51),key=51, & 
& nodecnc=[84,485,1580,486,2656,3057,4152,3058,5877, 5878,5879, 5880,5881, 5882,5883, 5884,5885, 5886 & 
& ,5887, 5888,5889, 5890,5891, 5892], & 
& edgecnc=[367,368,369,370,371,372,373,374], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(51),elname="xbrick",eltype="xbrick",typekey=51) 

        call prepare(lib_xbrick(52),key=52, & 
& nodecnc=[1513,488,85,1504,4085,3060,2657,4076,5893, 5894,5895, 5896,5897, 5898,5899, 5900,5901, 5902 & 
& ,5903, 5904,5905, 5906,5907, 5908], & 
& edgecnc=[375,376,377,378,379,380,381,382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(52),elname="xbrick",eltype="xbrick",typekey=52) 

        call prepare(lib_xbrick(53),key=53, & 
& nodecnc=[1507,86,488,1513,4079,2658,3060,4085,5909, 5910,5911, 5912,5894, 5893,5913, 5914,5915, 5916 & 
& ,5917, 5918,5902, 5901,5919, 5920], & 
& edgecnc=[383,384,375,385,386,387,379,388], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(53),elname="xbrick",eltype="xbrick",typekey=53) 

        call prepare(lib_xbrick(54),key=54, & 
& nodecnc=[1504,1679,324,1513,4076,4251,2896,4085,5921, 5922,5923, 5924,5925, 5926,5900, 5899,5927, 5928 & 
& ,5929, 5930,5931, 5932,5908, 5907], & 
& edgecnc=[389,390,391,378,392,393,394,382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(54),elname="xbrick",eltype="xbrick",typekey=54) 

        call prepare(lib_xbrick(55),key=55, & 
& nodecnc=[2123,2051,2007,2102,4695,4623,4579,4674,5933, 5934,5935, 5936,5937, 5938,5939, 5940,5941, 5942 & 
& ,5943, 5944,5945, 5946,5947, 5948], & 
& edgecnc=[395,396,397,398,399,400,401,402], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(55),elname="xbrick",eltype="xbrick",typekey=55) 

        call prepare(lib_xbrick(56),key=56, & 
& nodecnc=[2052,2006,2100,287,4624,4578,4672,2859,5949, 5950,5951, 5952,5953, 5954,5955, 5956,5957, 5958 & 
& ,5959, 5960,5961, 5962,5963, 5964], & 
& edgecnc=[403,404,405,406,407,408,409,410], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(56),elname="xbrick",eltype="xbrick",typekey=56) 

        call prepare(lib_xbrick(57),key=57, & 
& nodecnc=[1976,1891,2356,1742,4548,4463,4928,4314,5965, 5966,5967, 5968,5969, 5970,5971, 5972,5973, 5974 & 
& ,5975, 5976,5977, 5978,5979, 5980], & 
& edgecnc=[411,412,413,414,415,416,417,418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(57),elname="xbrick",eltype="xbrick",typekey=57) 

        call prepare(lib_xbrick(58),key=58, & 
& nodecnc=[1522,1517,383,382,4094,4089,2955,2954,5981, 5982,5983, 5984,5985, 5986,5987, 5988,5989, 5990 & 
& ,5991, 5992,5993, 5994,5995, 5996], & 
& edgecnc=[419,420,421,422,423,424,425,426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(58),elname="xbrick",eltype="xbrick",typekey=58) 

        call prepare(lib_xbrick(59),key=59, & 
& nodecnc=[1518,1552,385,384,4090,4124,2957,2956,5997, 5998,5999, 6000,6001, 6002,6003, 6004,6005, 6006 & 
& ,6007, 6008,6009, 6010,6011, 6012], & 
& edgecnc=[427,428,429,430,431,432,433,434], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(59),elname="xbrick",eltype="xbrick",typekey=59) 

        call prepare(lib_xbrick(60),key=60, & 
& nodecnc=[1551,1518,384,29,4123,4090,2956,2601,6013, 6014,6004, 6003,6015, 6016,6017, 6018,6019, 6020 & 
& ,6012, 6011,6021, 6022,6023, 6024], & 
& edgecnc=[435,430,436,437,438,434,439,440], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(60),elname="xbrick",eltype="xbrick",typekey=60) 

        call prepare(lib_xbrick(61),key=61, & 
& nodecnc=[374,50,497,2206,2946,2622,3069,4778,6025, 6026,6027, 6028,6029, 6030,6031, 6032,6033, 6034 & 
& ,6035, 6036,6037, 6038,6039, 6040], & 
& edgecnc=[441,442,443,444,445,446,447,448], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(61),elname="xbrick",eltype="xbrick",typekey=61) 

        call prepare(lib_xbrick(62),key=62, & 
& nodecnc=[1845,256,1725,1784,4417,2828,4297,4356,6041, 6042,6043, 6044,6045, 6046,6047, 6048,6049, 6050 & 
& ,6051, 6052,6053, 6054,6055, 6056], & 
& edgecnc=[449,450,451,452,453,454,455,456], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(62),elname="xbrick",eltype="xbrick",typekey=62) 

        call prepare(lib_xbrick(63),key=63, & 
& nodecnc=[1845,1767,1714,256,4417,4339,4286,2828,6057, 6058,6059, 6060,6061, 6062,6042, 6041,6063, 6064 & 
& ,6065, 6066,6067, 6068,6050, 6049], & 
& edgecnc=[457,458,459,449,460,461,462,453], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(63),elname="xbrick",eltype="xbrick",typekey=63) 

        call prepare(lib_xbrick(64),key=64, & 
& nodecnc=[1831,2357,1901,1734,4403,4929,4473,4306,6069, 6070,6071, 6072,6073, 6074,6075, 6076,6077, 6078 & 
& ,6079, 6080,6081, 6082,6083, 6084], & 
& edgecnc=[463,464,465,466,467,468,469,470], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(64),elname="xbrick",eltype="xbrick",typekey=64) 

        call prepare(lib_xbrick(65),key=65, & 
& nodecnc=[2277,254,1871,1927,4849,2826,4443,4499,6085, 6086,6087, 6088,6089, 6090,6091, 6092,6093, 6094 & 
& ,6095, 6096,6097, 6098,6099, 6100], & 
& edgecnc=[471,472,473,474,475,476,477,478], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(65),elname="xbrick",eltype="xbrick",typekey=65) 

        call prepare(lib_xbrick(66),key=66, & 
& nodecnc=[2006,1927,1871,288,4578,4499,4443,2860,6101, 6102,6090, 6089,6103, 6104,6105, 6106,6107, 6108 & 
& ,6098, 6097,6109, 6110,6111, 6112], & 
& edgecnc=[479,473,480,481,482,477,483,484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(66),elname="xbrick",eltype="xbrick",typekey=66) 

        call prepare(lib_xbrick(67),key=67, & 
& nodecnc=[2100,2006,288,2053,4672,4578,2860,4625,5952, 5951,6106, 6105,6113, 6114,6115, 6116,5960, 5959 & 
& ,6112, 6111,6117, 6118,6119, 6120], & 
& edgecnc=[404,481,485,486,408,484,487,488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(67),elname="xbrick",eltype="xbrick",typekey=67) 

        call prepare(lib_xbrick(68),key=68, & 
& nodecnc=[193,2092,2411,2484,2765,4664,4983,5056,6121, 6122,6123, 6124,6125, 6126,6127, 6128,6129, 6130 & 
& ,6131, 6132,6133, 6134,6135, 6136], & 
& edgecnc=[489,490,491,492,493,494,495,496], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(68),elname="xbrick",eltype="xbrick",typekey=68) 

        call prepare(lib_xbrick(69),key=69, & 
& nodecnc=[1963,2328,1946,2014,4535,4900,4518,4586,6137, 6138,6139, 6140,6141, 6142,6143, 6144,6145, 6146 & 
& ,6147, 6148,6149, 6150,6151, 6152], & 
& edgecnc=[497,498,499,500,501,502,503,504], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(69),elname="xbrick",eltype="xbrick",typekey=69) 

        call prepare(lib_xbrick(70),key=70, & 
& nodecnc=[1830,1768,1689,191,4402,4340,4261,2763,6153, 6154,6155, 6156,6157, 6158,6159, 6160,6161, 6162 & 
& ,6163, 6164,6165, 6166,6167, 6168], & 
& edgecnc=[505,506,507,508,509,510,511,512], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(70),elname="xbrick",eltype="xbrick",typekey=70) 

        call prepare(lib_xbrick(71),key=71, & 
& nodecnc=[1583,2221,191,1584,4155,4793,2763,4156,6169, 6170,6171, 6172,6173, 6174,6175, 6176,6177, 6178 & 
& ,6179, 6180,6181, 6182,6183, 6184], & 
& edgecnc=[513,514,515,516,517,518,519,520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(71),elname="xbrick",eltype="xbrick",typekey=71) 

        call prepare(lib_xbrick(72),key=72, & 
& nodecnc=[1545,90,495,1585,4117,2662,3067,4157,6185, 6186,6187, 6188,6189, 6190,6191, 6192,6193, 6194 & 
& ,6195, 6196,6197, 6198,6199, 6200], & 
& edgecnc=[521,522,523,524,525,526,527,528], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(72),elname="xbrick",eltype="xbrick",typekey=72) 

        call prepare(lib_xbrick(73),key=73, & 
& nodecnc=[1689,291,1585,1520,4261,2863,4157,4092,6201, 6202,6203, 6204,6205, 6206,6207, 6208,6209, 6210 & 
& ,6211, 6212,6213, 6214,6215, 6216], & 
& edgecnc=[529,530,531,532,533,534,535,536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(73),elname="xbrick",eltype="xbrick",typekey=73) 

        call prepare(lib_xbrick(74),key=74, & 
& nodecnc=[253,1549,1521,2101,2825,4121,4093,4673,6217, 6218,6219, 6220,6221, 6222,6223, 6224,6225, 6226 & 
& ,6227, 6228,6229, 6230,6231, 6232], & 
& edgecnc=[537,538,539,540,541,542,543,544], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(74),elname="xbrick",eltype="xbrick",typekey=74) 

        call prepare(lib_xbrick(75),key=75, & 
& nodecnc=[1521,1549,381,380,4093,4121,2953,2952,6220, 6219,6233, 6234,6235, 6236,6237, 6238,6228, 6227 & 
& ,6239, 6240,6241, 6242,6243, 6244], & 
& edgecnc=[538,545,546,547,542,548,549,550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(75),elname="xbrick",eltype="xbrick",typekey=75) 

        call prepare(lib_xbrick(76),key=76, & 
& nodecnc=[253,2066,1550,1549,2825,4638,4122,4121,6245, 6246,6247, 6248,6249, 6250,6218, 6217,6251, 6252 & 
& ,6253, 6254,6255, 6256,6226, 6225], & 
& edgecnc=[551,552,553,537,554,555,556,541], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(76),elname="xbrick",eltype="xbrick",typekey=76) 

        call prepare(lib_xbrick(77),key=77, & 
& nodecnc=[378,26,1481,1523,2950,2598,4053,4095,6257, 6258,6259, 6260,6261, 6262,6263, 6264,6265, 6266 & 
& ,6267, 6268,6269, 6270,6271, 6272], & 
& edgecnc=[557,558,559,560,561,562,563,564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(77),elname="xbrick",eltype="xbrick",typekey=77) 

        call prepare(lib_xbrick(78),key=78, & 
& nodecnc=[2219,1672,1547,1523,4791,4244,4119,4095,6273, 6274,6275, 6276,6277, 6278,6279, 6280,6281, 6282 & 
& ,6283, 6284,6285, 6286,6287, 6288], & 
& edgecnc=[565,566,567,568,569,570,571,572], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(78),elname="xbrick",eltype="xbrick",typekey=78) 

        call prepare(lib_xbrick(79),key=79, & 
& nodecnc=[1553,1524,386,30,4125,4096,2958,2602,6289, 6290,6291, 6292,6293, 6294,6295, 6296,6297, 6298 & 
& ,6299, 6300,6301, 6302,6303, 6304], & 
& edgecnc=[573,574,575,576,577,578,579,580], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(79),elname="xbrick",eltype="xbrick",typekey=79) 

        call prepare(lib_xbrick(80),key=80, & 
& nodecnc=[388,387,1525,1516,2960,2959,4097,4088,6305, 6306,6307, 6308,6309, 6310,5724, 5723,6311, 6312 & 
& ,6313, 6314,6315, 6316,5732, 5731], & 
& edgecnc=[581,582,583,290,584,585,586,294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(80),elname="xbrick",eltype="xbrick",typekey=80) 

        call prepare(lib_xbrick(81),key=81, & 
& nodecnc=[1526,1556,390,389,4098,4128,2962,2961,6317, 6318,6319, 6320,6321, 6322,6323, 6324,6325, 6326 & 
& ,6327, 6328,6329, 6330,6331, 6332], & 
& edgecnc=[587,588,589,590,591,592,593,594], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(81),elname="xbrick",eltype="xbrick",typekey=81) 

        call prepare(lib_xbrick(82),key=82, & 
& nodecnc=[1799,1556,1526,1715,4371,4128,4098,4287,6333, 6334,6318, 6317,6335, 6336,6337, 6338,6339, 6340 & 
& ,6326, 6325,6341, 6342,6343, 6344], & 
& edgecnc=[595,587,596,597,598,591,599,600], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(82),elname="xbrick",eltype="xbrick",typekey=82) 

        call prepare(lib_xbrick(83),key=83, & 
& nodecnc=[1527,1558,392,391,4099,4130,2964,2963,6345, 6346,6347, 6348,6349, 6350,6351, 6352,6353, 6354 & 
& ,6355, 6356,6357, 6358,6359, 6360], & 
& edgecnc=[601,602,603,604,605,606,607,608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(83),elname="xbrick",eltype="xbrick",typekey=83) 

        call prepare(lib_xbrick(84),key=84, & 
& nodecnc=[35,394,1528,1188,2607,2966,4100,3760,6361, 6362,6363, 6364,6365, 6366,6367, 6368,6369, 6370 & 
& ,6371, 6372,6373, 6374,6375, 6376], & 
& edgecnc=[609,610,611,612,613,614,615,616], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(84),elname="xbrick",eltype="xbrick",typekey=84) 

        call prepare(lib_xbrick(85),key=85, & 
& nodecnc=[395,35,1188,1218,2967,2607,3760,3790,6377, 6378,6368, 6367,6379, 6380,6381, 6382,6383, 6384 & 
& ,6376, 6375,6385, 6386,6387, 6388], & 
& edgecnc=[617,612,618,619,620,616,621,622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(85),elname="xbrick",eltype="xbrick",typekey=85) 

        call prepare(lib_xbrick(86),key=86, & 
& nodecnc=[1012,1011,1037,983,3584,3583,3609,3555,6389, 6390,6391, 6392,6393, 6394,6395, 6396,6397, 6398 & 
& ,6399, 6400,6401, 6402,6403, 6404], & 
& edgecnc=[623,624,625,626,627,628,629,630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(86),elname="xbrick",eltype="xbrick",typekey=86) 

        call prepare(lib_xbrick(87),key=87, & 
& nodecnc=[1038,988,2423,1048,3610,3560,4995,3620,6405, 6406,6407, 6408,6409, 6410,6411, 6412,6413, 6414 & 
& ,6415, 6416,6417, 6418,6419, 6420], & 
& edgecnc=[631,632,633,634,635,636,637,638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(87),elname="xbrick",eltype="xbrick",typekey=87) 

        call prepare(lib_xbrick(88),key=88, & 
& nodecnc=[2394,797,837,779,4966,3369,3409,3351,6421, 6422,6423, 6424,6425, 6426,6427, 6428,6429, 6430 & 
& ,6431, 6432,6433, 6434,6435, 6436], & 
& edgecnc=[639,640,641,642,643,644,645,646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(88),elname="xbrick",eltype="xbrick",typekey=88) 

        call prepare(lib_xbrick(89),key=89, & 
& nodecnc=[2397,861,909,2404,4969,3433,3481,4976,6437, 6438,6439, 6440,6441, 6442,6443, 6444,6445, 6446 & 
& ,6447, 6448,6449, 6450,6451, 6452], & 
& edgecnc=[647,648,649,650,651,652,653,654], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(89),elname="xbrick",eltype="xbrick",typekey=89) 

        call prepare(lib_xbrick(90),key=90, & 
& nodecnc=[861,821,95,887,3433,3393,2667,3459,6453, 6454,6455, 6456,6457, 6458,6459, 6460,6461, 6462,6463 & 
& , 6464,6465, 6466,6467, 6468], & 
& edgecnc=[655,656,657,658,659,660,661,662], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(90),elname="xbrick",eltype="xbrick",typekey=90) 

        call prepare(lib_xbrick(91),key=91, & 
& nodecnc=[1033,2520,1065,1009,3605,5092,3637,3581,6469, 6470,6471, 6472,6473, 6474,6475, 6476,6477, 6478 & 
& ,6479, 6480,6481, 6482,6483, 6484], & 
& edgecnc=[663,664,665,666,667,668,669,670], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(91),elname="xbrick",eltype="xbrick",typekey=91) 

        call prepare(lib_xbrick(92),key=92, & 
& nodecnc=[1087,1614,142,1114,3659,4186,2714,3686,6485, 6486,6487, 6488,6489, 6490,6491, 6492,6493, 6494 & 
& ,6495, 6496,6497, 6498,6499, 6500], & 
& edgecnc=[671,672,673,674,675,676,677,678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(92),elname="xbrick",eltype="xbrick",typekey=92) 

        call prepare(lib_xbrick(93),key=93, & 
& nodecnc=[1080,142,1119,1059,3652,2714,3691,3631,6501, 6502,6503, 6504,6505, 6506,6507, 6508,6509, 6510 & 
& ,6511, 6512,6513, 6514,6515, 6516], & 
& edgecnc=[679,680,681,682,683,684,685,686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(93),elname="xbrick",eltype="xbrick",typekey=93) 

        call prepare(lib_xbrick(94),key=94, & 
& nodecnc=[1708,1464,1501,1611,4280,4036,4073,4183,6517, 6518,6519, 6520,6521, 6522,6523, 6524,6525, 6526 & 
& ,6527, 6528,6529, 6530,6531, 6532], & 
& edgecnc=[687,688,689,690,691,692,693,694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(94),elname="xbrick",eltype="xbrick",typekey=94) 

        call prepare(lib_xbrick(95),key=95, & 
& nodecnc=[1053,2283,1013,1074,3625,4855,3585,3646,6533, 6534,6535, 6536,6537, 6538,6539, 6540,6541, 6542 & 
& ,6543, 6544,6545, 6546,6547, 6548], & 
& edgecnc=[695,696,697,698,699,700,701,702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(95),elname="xbrick",eltype="xbrick",typekey=95) 

        call prepare(lib_xbrick(96),key=96, & 
& nodecnc=[276,904,833,873,2848,3476,3405,3445,6549, 6550,6551, 6552,6553, 6554,6555, 6556,6557, 6558 & 
& ,6559, 6560,6561, 6562,6563, 6564], & 
& edgecnc=[703,704,705,706,707,708,709,710], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(96),elname="xbrick",eltype="xbrick",typekey=96) 

        call prepare(lib_xbrick(97),key=97, & 
& nodecnc=[606,648,724,2309,3178,3220,3296,4881,6565, 6566,6567, 6568,6569, 6570,6571, 6572,6573, 6574 & 
& ,6575, 6576,6577, 6578,6579, 6580], & 
& edgecnc=[711,712,713,714,715,716,717,718], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(97),elname="xbrick",eltype="xbrick",typekey=97) 

        call prepare(lib_xbrick(98),key=98, & 
& nodecnc=[499,502,543,569,3071,3074,3115,3141,5494, 5493,6581, 6582,6583, 6584,5512, 5511,5500, 5499 & 
& ,6585, 6586,6587, 6588,5520, 5519], & 
& edgecnc=[175,719,720,184,178,721,722,188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(98),elname="xbrick",eltype="xbrick",typekey=98) 

        call prepare(lib_xbrick(99),key=99, & 
& nodecnc=[1465,397,36,1464,4037,2969,2608,4036,6589, 6590,6591, 6592,6593, 6594,6595, 6596,6597, 6598 & 
& ,6599, 6600,6601, 6602,6603, 6604], & 
& edgecnc=[723,724,725,726,727,728,729,730], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(99),elname="xbrick",eltype="xbrick",typekey=99) 

        call prepare(lib_xbrick(100),key=100, & 
& nodecnc=[878,879,275,889,3450,3451,2847,3461,6605, 6606,6607, 6608,6609, 6610,6611, 6612,6613, 6614 & 
& ,6615, 6616,6617, 6618,6619, 6620], & 
& edgecnc=[731,732,733,734,735,736,737,738], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(100),elname="xbrick",eltype="xbrick",typekey=100) 

        call prepare(lib_xbrick(101),key=101, & 
& nodecnc=[2313,705,2371,741,4885,3277,4943,3313,6621, 6622,6623, 6624,6625, 6626,6627, 6628,6629, 6630 & 
& ,6631, 6632,6633, 6634,6635, 6636], & 
& edgecnc=[739,740,741,742,743,744,745,746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(101),elname="xbrick",eltype="xbrick",typekey=101) 

        call prepare(lib_xbrick(102),key=102, & 
& nodecnc=[705,745,776,2371,3277,3317,3348,4943,6637, 6638,6639, 6640,6641, 6642,6624, 6623,6643, 6644 & 
& ,6645, 6646,6647, 6648,6632, 6631], & 
& edgecnc=[747,748,749,740,750,751,752,744], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(102),elname="xbrick",eltype="xbrick",typekey=102) 

        call prepare(lib_xbrick(103),key=103, & 
& nodecnc=[37,1466,1472,399,2609,4038,4044,2971,6649, 6650,6651, 6652,6653, 6654,6655, 6656,6657, 6658 & 
& ,6659, 6660,6661, 6662,6663, 6664], & 
& edgecnc=[753,754,755,756,757,758,759,760], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(103),elname="xbrick",eltype="xbrick",typekey=103) 

        call prepare(lib_xbrick(104),key=104, & 
& nodecnc=[1032,897,989,997,3604,3469,3561,3569,6665, 6666,6667, 6668,6669, 6670,6671, 6672,6673, 6674 & 
& ,6675, 6676,6677, 6678,6679, 6680], & 
& edgecnc=[761,762,763,764,765,766,767,768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(104),elname="xbrick",eltype="xbrick",typekey=104) 

        call prepare(lib_xbrick(105),key=105, & 
& nodecnc=[744,2317,700,2350,3316,4889,3272,4922,6681, 6682,6683, 6684,6685, 6686,6687, 6688,6689, 6690 & 
& ,6691, 6692,6693, 6694,6695, 6696], & 
& edgecnc=[769,770,771,772,773,774,775,776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(105),elname="xbrick",eltype="xbrick",typekey=105) 

        call prepare(lib_xbrick(106),key=106, & 
& nodecnc=[520,508,554,549,3092,3080,3126,3121,6697, 6698,6699, 6700,6701, 6702,6703, 6704,6705, 6706 & 
& ,6707, 6708,6709, 6710,6711, 6712], & 
& edgecnc=[777,778,779,780,781,782,783,784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(106),elname="xbrick",eltype="xbrick",typekey=106) 

        call prepare(lib_xbrick(107),key=107, & 
& nodecnc=[1798,1576,1539,1716,4370,4148,4111,4288,6713, 6714,6715, 6716,6717, 6718,6719, 6720,6721, 6722 & 
& ,6723, 6724,6725, 6726,6727, 6728], & 
& edgecnc=[785,786,787,788,789,790,791,792], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(107),elname="xbrick",eltype="xbrick",typekey=107) 

        call prepare(lib_xbrick(108),key=108, & 
& nodecnc=[1756,1796,1798,1716,4328,4368,4370,4288,6729, 6730,6731, 6732,6720, 6719,6733, 6734,6735, 6736 & 
& ,6737, 6738,6728, 6727,6739, 6740], & 
& edgecnc=[793,794,788,795,796,797,792,798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(108),elname="xbrick",eltype="xbrick",typekey=108) 

        call prepare(lib_xbrick(109),key=109, & 
& nodecnc=[1804,1858,1885,2547,4376,4430,4457,5119,6741, 6742,6743, 6744,6745, 6746,6747, 6748,6749, 6750 & 
& ,6751, 6752,6753, 6754,6755, 6756], & 
& edgecnc=[799,800,801,802,803,804,805,806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(109),elname="xbrick",eltype="xbrick",typekey=109) 

        call prepare(lib_xbrick(110),key=110, & 
& nodecnc=[2064,2533,2105,2552,4636,5105,4677,5124,6757, 6758,6759, 6760,6761, 6762,6763, 6764,6765, 6766 & 
& ,6767, 6768,6769, 6770,6771, 6772], & 
& edgecnc=[807,808,809,810,811,812,813,814], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(110),elname="xbrick",eltype="xbrick",typekey=110) 

        call prepare(lib_xbrick(111),key=111, & 
& nodecnc=[1854,1886,184,1965,4426,4458,2756,4537,6773, 6774,6775, 6776,6777, 6778,6779, 6780,6781, 6782 & 
& ,6783, 6784,6785, 6786,6787, 6788], & 
& edgecnc=[815,816,817,818,819,820,821,822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(111),elname="xbrick",eltype="xbrick",typekey=111) 

        call prepare(lib_xbrick(112),key=112, & 
& nodecnc=[1854,1965,2019,217,4426,4537,4591,2789,6780, 6779,6789, 6790,6791, 6792,6793, 6794,6788, 6787 & 
& ,6795, 6796,6797, 6798,6799, 6800], & 
& edgecnc=[818,823,824,825,822,826,827,828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(112),elname="xbrick",eltype="xbrick",typekey=112) 

        call prepare(lib_xbrick(113),key=113, & 
& nodecnc=[2227,1758,1800,1718,4799,4330,4372,4290,6801, 6802,6803, 6804,6805, 6806,6807, 6808,6809, 6810 & 
& ,6811, 6812,6813, 6814,6815, 6816], & 
& edgecnc=[829,830,831,832,833,834,835,836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(113),elname="xbrick",eltype="xbrick",typekey=113) 

        call prepare(lib_xbrick(114),key=114, & 
& nodecnc=[229,1682,1586,1489,2801,4254,4158,4061,6817, 6818,6819, 6820,6821, 6822,6823, 6824,6825, 6826 & 
& ,6827, 6828,6829, 6830,6831, 6832], & 
& edgecnc=[837,838,839,840,841,842,843,844], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(114),elname="xbrick",eltype="xbrick",typekey=114) 

        call prepare(lib_xbrick(115),key=115, & 
& nodecnc=[416,46,1489,1586,2988,2618,4061,4158,6833, 6834,6835, 6836,6822, 6821,6837, 6838,6839, 6840 & 
& ,6841, 6842,6830, 6829,6843, 6844], & 
& edgecnc=[845,846,839,847,848,849,843,850], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(115),elname="xbrick",eltype="xbrick",typekey=115) 

        call prepare(lib_xbrick(116),key=116, & 
& nodecnc=[228,2104,1598,1597,2800,4676,4170,4169,6845, 6846,6847, 6848,6849, 6850,6851, 6852,6853, 6854 & 
& ,6855, 6856,6857, 6858,6859, 6860], & 
& edgecnc=[851,852,853,854,855,856,857,858], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(116),elname="xbrick",eltype="xbrick",typekey=116) 

        call prepare(lib_xbrick(117),key=117, & 
& nodecnc=[1587,1599,419,418,4159,4171,2991,2990,6861, 6862,6863, 6864,6865, 6866,6867, 6868,6869, 6870 & 
& ,6871, 6872,6873, 6874,6875, 6876], & 
& edgecnc=[859,860,861,862,863,864,865,866], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(117),elname="xbrick",eltype="xbrick",typekey=117) 

        call prepare(lib_xbrick(118),key=118, & 
& nodecnc=[260,1825,1740,1893,2832,4397,4312,4465,6877, 6878,6879, 6880,6881, 6882,6883, 6884,6885, 6886 & 
& ,6887, 6888,6889, 6890,6891, 6892], & 
& edgecnc=[867,868,869,870,871,872,873,874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(118),elname="xbrick",eltype="xbrick",typekey=118) 

        call prepare(lib_xbrick(119),key=119, & 
& nodecnc=[2145,222,2035,2087,4717,2794,4607,4659,6893, 6894,6895, 6896,6897, 6898,6899, 6900,6901, 6902 & 
& ,6903, 6904,6905, 6906,6907, 6908], & 
& edgecnc=[875,876,877,878,879,880,881,882], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(119),elname="xbrick",eltype="xbrick",typekey=119) 

        call prepare(lib_xbrick(120),key=120, & 
& nodecnc=[2103,2049,1925,2005,4675,4621,4497,4577,6909, 6910,6911, 6912,6913, 6914,6915, 6916,6917, 6918 & 
& ,6919, 6920,6921, 6922,6923, 6924], & 
& edgecnc=[883,884,885,886,887,888,889,890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(120),elname="xbrick",eltype="xbrick",typekey=120) 

        call prepare(lib_xbrick(121),key=121, & 
& nodecnc=[2152,2036,2089,2146,4724,4608,4661,4718,6925, 6926,6927, 6928,6929, 6930,6931, 6932,6933, 6934 & 
& ,6935, 6936,6937, 6938,6939, 6940], & 
& edgecnc=[891,892,893,894,895,896,897,898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(121),elname="xbrick",eltype="xbrick",typekey=121) 

        call prepare(lib_xbrick(122),key=122, & 
& nodecnc=[1844,1766,1713,326,4416,4338,4285,2898,6941, 6942,6943, 6944,6945, 6946,6947, 6948,6949, 6950 & 
& ,6951, 6952,6953, 6954,6955, 6956], & 
& edgecnc=[899,900,901,902,903,904,905,906], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(122),elname="xbrick",eltype="xbrick",typekey=122) 

        call prepare(lib_xbrick(123),key=123, & 
& nodecnc=[439,438,1588,1491,3011,3010,4160,4063,6957, 6958,6959, 6960,6961, 6962,6963, 6964,6965, 6966 & 
& ,6967, 6968,6969, 6970,6971, 6972], & 
& edgecnc=[907,908,909,910,911,912,913,914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(123),elname="xbrick",eltype="xbrick",typekey=123) 

        call prepare(lib_xbrick(124),key=124, & 
& nodecnc=[327,1492,1491,1713,2899,4064,4063,4285,6973, 6974,6975, 6976,6977, 6978,6979, 6980,6981, 6982 & 
& ,6983, 6984,6985, 6986,6987, 6988], & 
& edgecnc=[915,916,917,918,919,920,921,922], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(124),elname="xbrick",eltype="xbrick",typekey=124) 

        call prepare(lib_xbrick(125),key=125, & 
& nodecnc=[440,60,1492,1529,3012,2632,4064,4101,6989, 6990,6991, 6992,6993, 6994,6995, 6996,6997, 6998 & 
& ,6999, 7000,7001, 7002,7003, 7004], & 
& edgecnc=[923,924,925,926,927,928,929,930], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(125),elname="xbrick",eltype="xbrick",typekey=125) 

        call prepare(lib_xbrick(126),key=126, & 
& nodecnc=[2218,1673,1567,1529,4790,4245,4139,4101,7005, 7006,7007, 7008,7009, 7010,7011, 7012,7013, 7014 & 
& ,7015, 7016,7017, 7018,7019, 7020], & 
& edgecnc=[931,932,933,934,935,936,937,938], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(126),elname="xbrick",eltype="xbrick",typekey=126) 

        call prepare(lib_xbrick(127),key=127, & 
& nodecnc=[1947,294,1870,1899,4519,2866,4442,4471,7021, 7022,7023, 7024,7025, 7026,7027, 7028,7029, 7030 & 
& ,7031, 7032,7033, 7034,7035, 7036], & 
& edgecnc=[939,940,941,942,943,944,945,946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(127),elname="xbrick",eltype="xbrick",typekey=127) 

        call prepare(lib_xbrick(128),key=128, & 
& nodecnc=[2015,2036,2152,2088,4587,4608,4724,4660,7037, 7038,6926, 6925,7039, 7040,7041, 7042,7043, 7044 & 
& ,6934, 6933,7045, 7046,7047, 7048], & 
& edgecnc=[947,891,948,949,950,895,951,952], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(128),elname="xbrick",eltype="xbrick",typekey=128) 

        call prepare(lib_xbrick(129),key=129, & 
& nodecnc=[171,2183,2165,2146,2743,4755,4737,4718,7049, 7050,7051, 7052,7053, 7054,7055, 7056,7057, 7058 & 
& ,7059, 7060,7061, 7062,7063, 7064], & 
& edgecnc=[953,954,955,956,957,958,959,960], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(129),elname="xbrick",eltype="xbrick",typekey=129) 

        call prepare(lib_xbrick(130),key=130, & 
& nodecnc=[2453,2197,2163,2437,5025,4769,4735,5009,7065, 7066,7067, 7068,7069, 7070,7071, 7072,7073, 7074 & 
& ,7075, 7076,7077, 7078,7079, 7080], & 
& edgecnc=[961,962,963,964,965,966,967,968], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(130),elname="xbrick",eltype="xbrick",typekey=130) 

        call prepare(lib_xbrick(131),key=131, & 
& nodecnc=[2196,2185,2192,2182,4768,4757,4764,4754,7081, 7082,7083, 7084,7085, 7086,7087, 7088,7089, 7090 & 
& ,7091, 7092,7093, 7094,7095, 7096], & 
& edgecnc=[969,970,971,972,973,974,975,976], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(131),elname="xbrick",eltype="xbrick",typekey=131) 

        call prepare(lib_xbrick(132),key=132, & 
& nodecnc=[2237,2238,2173,2139,4809,4810,4745,4711,7097, 7098,7099, 7100,7101, 7102,7103, 7104,7105, 7106 & 
& ,7107, 7108,7109, 7110,7111, 7112], & 
& edgecnc=[977,978,979,980,981,982,983,984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(132),elname="xbrick",eltype="xbrick",typekey=132) 

        call prepare(lib_xbrick(133),key=133, & 
& nodecnc=[228,2058,2059,2104,2800,4630,4631,4676,7113, 7114,7115, 7116,7117, 7118,6846, 6845,7119, 7120 & 
& ,7121, 7122,7123, 7124,6854, 6853], & 
& edgecnc=[985,986,987,851,988,989,990,855], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(133),elname="xbrick",eltype="xbrick",typekey=133) 

        call prepare(lib_xbrick(134),key=134, & 
& nodecnc=[2237,2115,2059,2058,4809,4687,4631,4630,7125, 7126,7127, 7128,7116, 7115,7129, 7130,7131, 7132 & 
& ,7133, 7134,7122, 7121,7135, 7136], & 
& edgecnc=[991,992,986,993,994,995,989,996], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(134),elname="xbrick",eltype="xbrick",typekey=134) 

        call prepare(lib_xbrick(135),key=135, & 
& nodecnc=[1978,1740,1694,227,4550,4312,4266,2799,7137, 7138,7139, 7140,7141, 7142,7143, 7144,7145, 7146 & 
& ,7147, 7148,7149, 7150,7151, 7152], & 
& edgecnc=[997,998,999,1000,1001,1002,1003,1004], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(135),elname="xbrick",eltype="xbrick",typekey=135) 

        call prepare(lib_xbrick(136),key=136, & 
& nodecnc=[1730,336,1701,1747,4302,2908,4273,4319,7153, 7154,7155, 7156,7157, 7158,7159, 7160,7161, 7162 & 
& ,7163, 7164,7165, 7166,7167, 7168], & 
& edgecnc=[1005,1006,1007,1008,1009,1010,1011,1012], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(136),elname="xbrick",eltype="xbrick",typekey=136) 

        call prepare(lib_xbrick(137),key=137, & 
& nodecnc=[1315,2440,1292,1638,3887,5012,3864,4210,7169, 7170,7171, 7172,7173, 7174,7175, 7176,7177, 7178 & 
& ,7179, 7180,7181, 7182,7183, 7184], & 
& edgecnc=[1013,1014,1015,1016,1017,1018,1019,1020], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(137),elname="xbrick",eltype="xbrick",typekey=137) 

        call prepare(lib_xbrick(138),key=138, & 
& nodecnc=[1755,2571,1709,1804,4327,5143,4281,4376,7185, 7186,7187, 7188,7189, 7190,7191, 7192,7193, 7194 & 
& ,7195, 7196,7197, 7198,7199, 7200], & 
& edgecnc=[1021,1022,1023,1024,1025,1026,1027,1028], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(138),elname="xbrick",eltype="xbrick",typekey=138) 

        call prepare(lib_xbrick(139),key=139, & 
& nodecnc=[165,1418,1407,1425,2737,3990,3979,3997,7201, 7202,7203, 7204,7205, 7206,7207, 7208,7209, 7210 & 
& ,7211, 7212,7213, 7214,7215, 7216], & 
& edgecnc=[1029,1030,1031,1032,1033,1034,1035,1036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(139),elname="xbrick",eltype="xbrick",typekey=139) 

        call prepare(lib_xbrick(140),key=140, & 
& nodecnc=[1854,217,1793,1809,4426,2789,4365,4381,6794, 6793,7217, 7218,7219, 7220,7221, 7222,6800, 6799 & 
& ,7223, 7224,7225, 7226,7227, 7228], & 
& edgecnc=[825,1037,1038,1039,828,1040,1041,1042], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(140),elname="xbrick",eltype="xbrick",typekey=140) 

        call prepare(lib_xbrick(141),key=141, & 
& nodecnc=[217,1916,1853,1793,2789,4488,4425,4365,7229, 7230,7231, 7232,7233, 7234,7218, 7217,7235, 7236 & 
& ,7237, 7238,7239, 7240,7224, 7223], & 
& edgecnc=[1043,1044,1045,1037,1046,1047,1048,1040], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(141),elname="xbrick",eltype="xbrick",typekey=141) 

        call prepare(lib_xbrick(142),key=142, & 
& nodecnc=[1776,2228,230,1728,4348,4800,2802,4300,7241, 7242,7243, 7244,7245, 7246,7247, 7248,7249, 7250 & 
& ,7251, 7252,7253, 7254,7255, 7256], & 
& edgecnc=[1049,1050,1051,1052,1053,1054,1055,1056], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(142),elname="xbrick",eltype="xbrick",typekey=142) 

        call prepare(lib_xbrick(143),key=143, & 
& nodecnc=[1467,1488,415,414,4039,4060,2987,2986,7257, 7258,7259, 7260,7261, 7262,7263, 7264,7265, 7266 & 
& ,7267, 7268,7269, 7270,7271, 7272], & 
& edgecnc=[1057,1058,1059,1060,1061,1062,1063,1064], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(143),elname="xbrick",eltype="xbrick",typekey=143) 

        call prepare(lib_xbrick(144),key=144, & 
& nodecnc=[1800,1488,1467,1718,4372,4060,4039,4290,7273, 7274,7258, 7257,7275, 7276,6806, 6805,7277, 7278 & 
& ,7266, 7265,7279, 7280,6814, 6813], & 
& edgecnc=[1065,1057,1066,831,1067,1061,1068,835], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(144),elname="xbrick",eltype="xbrick",typekey=144) 

        call prepare(lib_xbrick(145),key=145, & 
& nodecnc=[1805,1704,1653,1700,4377,4276,4225,4272,7281, 7282,7283, 7284,7285, 7286,7287, 7288,7289, 7290 & 
& ,7291, 7292,7293, 7294,7295, 7296], & 
& edgecnc=[1069,1070,1071,1072,1073,1074,1075,1076], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(145),elname="xbrick",eltype="xbrick",typekey=145) 

        call prepare(lib_xbrick(146),key=146, & 
& nodecnc=[1850,231,1675,1787,4422,2803,4247,4359,7297, 7298,7299, 7300,7301, 7302,7303, 7304,7305, 7306 & 
& ,7307, 7308,7309, 7310,7311, 7312], & 
& edgecnc=[1077,1078,1079,1080,1081,1082,1083,1084], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(146),elname="xbrick",eltype="xbrick",typekey=146) 

        call prepare(lib_xbrick(147),key=147, & 
& nodecnc=[45,413,1468,1487,2617,2985,4040,4059,7313, 7314,7315, 7316,7317, 7318,7319, 7320,7321, 7322 & 
& ,7323, 7324,7325, 7326,7327, 7328], & 
& edgecnc=[1085,1086,1087,1088,1089,1090,1091,1092], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(147),elname="xbrick",eltype="xbrick",typekey=147) 

        call prepare(lib_xbrick(148),key=148, & 
& nodecnc=[1850,1776,1728,231,4422,4348,4300,2803,7329, 7330,7248, 7247,7331, 7332,7298, 7297,7333, 7334 & 
& ,7256, 7255,7335, 7336,7306, 7305], & 
& edgecnc=[1093,1052,1094,1077,1095,1056,1096,1081], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(148),elname="xbrick",eltype="xbrick",typekey=148) 

        call prepare(lib_xbrick(149),key=149, & 
& nodecnc=[1861,2234,267,1703,4433,4806,2839,4275,7337, 7338,7339, 7340,7341, 7342,7343, 7344,7345, 7346 & 
& ,7347, 7348,7349, 7350,7351, 7352], & 
& edgecnc=[1097,1098,1099,1100,1101,1102,1103,1104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(149),elname="xbrick",eltype="xbrick",typekey=149) 

        call prepare(lib_xbrick(150),key=150, & 
& nodecnc=[232,1675,1486,1470,2804,4247,4058,4042,7353, 7354,7355, 7356,7357, 7358,7359, 7360,7361, 7362 & 
& ,7363, 7364,7365, 7366,7367, 7368], & 
& edgecnc=[1105,1106,1107,1108,1109,1110,1111,1112], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(150),elname="xbrick",eltype="xbrick",typekey=150) 

        call prepare(lib_xbrick(151),key=151, & 
& nodecnc=[413,412,1469,1468,2985,2984,4041,4040,7369, 7370,7371, 7372,7373, 7374,7316, 7315,7375, 7376 & 
& ,7377, 7378,7379, 7380,7324, 7323], & 
& edgecnc=[1113,1114,1115,1086,1116,1117,1118,1090], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(151),elname="xbrick",eltype="xbrick",typekey=151) 

        call prepare(lib_xbrick(152),key=152, & 
& nodecnc=[233,1674,1485,1484,2805,4246,4057,4056,7381, 7382,7383, 7384,7385, 7386,7387, 7388,7389, 7390 & 
& ,7391, 7392,7393, 7394,7395, 7396], & 
& edgecnc=[1119,1120,1121,1122,1123,1124,1125,1126], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(152),elname="xbrick",eltype="xbrick",typekey=152) 

        call prepare(lib_xbrick(153),key=153, & 
& nodecnc=[44,411,1470,1486,2616,2983,4042,4058,7397, 7398,7399, 7400,7358, 7357,7401, 7402,7403, 7404 & 
& ,7405, 7406,7366, 7365,7407, 7408], & 
& edgecnc=[1127,1128,1107,1129,1130,1131,1111,1132], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(153),elname="xbrick",eltype="xbrick",typekey=153) 

        call prepare(lib_xbrick(154),key=154, & 
& nodecnc=[1589,1600,420,48,4161,4172,2992,2620,7409, 7410,7411, 7412,7413, 7414,7415, 7416,7417, 7418 & 
& ,7419, 7420,7421, 7422,7423, 7424], & 
& edgecnc=[1133,1134,1135,1136,1137,1138,1139,1140], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(154),elname="xbrick",eltype="xbrick",typekey=154) 

        call prepare(lib_xbrick(155),key=155, & 
& nodecnc=[1840,225,1711,1763,4412,2797,4283,4335,7425, 7426,7427, 7428,7429, 7430,7431, 7432,7433, 7434 & 
& ,7435, 7436,7437, 7438,7439, 7440], & 
& edgecnc=[1141,1142,1143,1144,1145,1146,1147,1148], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(155),elname="xbrick",eltype="xbrick",typekey=155) 

        call prepare(lib_xbrick(156),key=156, & 
& nodecnc=[2354,223,2010,2038,4926,2795,4582,4610,7441, 7442,7443, 7444,7445, 7446,7447, 7448,7449, 7450 & 
& ,7451, 7452,7453, 7454,7455, 7456], & 
& edgecnc=[1149,1150,1151,1152,1153,1154,1155,1156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(156),elname="xbrick",eltype="xbrick",typekey=156) 

        call prepare(lib_xbrick(157),key=157, & 
& nodecnc=[189,1980,1738,1897,2761,4552,4310,4469,7457, 7458,7459, 7460,7461, 7462,7463, 7464,7465, 7466 & 
& ,7467, 7468,7469, 7470,7471, 7472], & 
& edgecnc=[1157,1158,1159,1160,1161,1162,1163,1164], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(157),elname="xbrick",eltype="xbrick",typekey=157) 

        call prepare(lib_xbrick(158),key=158, & 
& nodecnc=[173,2245,1925,1868,2745,4817,4497,4440,7473, 7474,7475, 7476,7477, 7478,7479, 7480,7481, 7482 & 
& ,7483, 7484,7485, 7486,7487, 7488], & 
& edgecnc=[1165,1166,1167,1168,1169,1170,1171,1172], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(158),elname="xbrick",eltype="xbrick",typekey=158) 

        call prepare(lib_xbrick(159),key=159, & 
& nodecnc=[1948,1898,1868,172,4520,4470,4440,2744,7489, 7490,7491, 7492,7493, 7494,7495, 7496,7497, 7498 & 
& ,7499, 7500,7501, 7502,7503, 7504], & 
& edgecnc=[1173,1174,1175,1176,1177,1178,1179,1180], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(159),elname="xbrick",eltype="xbrick",typekey=159) 

        call prepare(lib_xbrick(160),key=160, & 
& nodecnc=[1843,292,1687,1765,4415,2864,4259,4337,7505, 7506,7507, 7508,7509, 7510,7511, 7512,7513, 7514 & 
& ,7515, 7516,7517, 7518,7519, 7520], & 
& edgecnc=[1181,1182,1183,1184,1185,1186,1187,1188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(160),elname="xbrick",eltype="xbrick",typekey=160) 

        call prepare(lib_xbrick(161),key=161, & 
& nodecnc=[1844,326,1724,1783,4416,2898,4296,4355,6948, 6947,7521, 7522,7523, 7524,7525, 7526,6956, 6955 & 
& ,7527, 7528,7529, 7530,7531, 7532], & 
& edgecnc=[902,1189,1190,1191,906,1192,1193,1194], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(161),elname="xbrick",eltype="xbrick",typekey=161) 

        call prepare(lib_xbrick(162),key=162, & 
& nodecnc=[2205,1603,437,436,4777,4175,3009,3008,7533, 7534,7535, 7536,7537, 7538,7539, 7540,7541, 7542 & 
& ,7543, 7544,7545, 7546,7547, 7548], & 
& edgecnc=[1195,1196,1197,1198,1199,1200,1201,1202], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(162),elname="xbrick",eltype="xbrick",typekey=162) 

        call prepare(lib_xbrick(163),key=163, & 
& nodecnc=[326,1588,1490,1724,2898,4160,4062,4296,7549, 7550,7551, 7552,7553, 7554,7522, 7521,7555, 7556 & 
& ,7557, 7558,7559, 7560,7528, 7527], & 
& edgecnc=[1203,1204,1205,1189,1206,1207,1208,1192], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(163),elname="xbrick",eltype="xbrick",typekey=163) 

        call prepare(lib_xbrick(164),key=164, & 
& nodecnc=[49,1590,1602,422,2621,4162,4174,2994,7561, 7562,7563, 7564,7565, 7566,7567, 7568,7569, 7570 & 
& ,7571, 7572,7573, 7574,7575, 7576], & 
& edgecnc=[1209,1210,1211,1212,1213,1214,1215,1216], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(164),elname="xbrick",eltype="xbrick",typekey=164) 

        call prepare(lib_xbrick(165),key=165, & 
& nodecnc=[51,423,1560,424,2623,2995,4132,2996,7577, 7578,7579, 7580,7581, 7582,7583, 7584,7585, 7586 & 
& ,7587, 7588,7589, 7590,7591, 7592], & 
& edgecnc=[1217,1218,1219,1220,1221,1222,1223,1224], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(165),elname="xbrick",eltype="xbrick",typekey=165) 

        call prepare(lib_xbrick(166),key=166, & 
& nodecnc=[1508,224,1676,1505,4080,2796,4248,4077,7593, 7594,7595, 7596,7597, 7598,7599, 7600,7601, 7602 & 
& ,7603, 7604,7605, 7606,7607, 7608], & 
& edgecnc=[1225,1226,1227,1228,1229,1230,1231,1232], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(166),elname="xbrick",eltype="xbrick",typekey=166) 

        call prepare(lib_xbrick(167),key=167, & 
& nodecnc=[1842,1764,1677,258,4414,4336,4249,2830,7609, 7610,7611, 7612,7613, 7614,7615, 7616,7617, 7618 & 
& ,7619, 7620,7621, 7622,7623, 7624], & 
& edgecnc=[1233,1234,1235,1236,1237,1238,1239,1240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(167),elname="xbrick",eltype="xbrick",typekey=167) 

        call prepare(lib_xbrick(168),key=168, & 
& nodecnc=[1676,224,1696,1782,4248,2796,4268,4354,7596, 7595,7625, 7626,7627, 7628,7629, 7630,7604, 7603 & 
& ,7631, 7632,7633, 7634,7635, 7636], & 
& edgecnc=[1226,1241,1242,1243,1230,1244,1245,1246], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(168),elname="xbrick",eltype="xbrick",typekey=168) 

        call prepare(lib_xbrick(169),key=169, & 
& nodecnc=[1711,225,1601,1600,4283,2797,4173,4172,7428, 7427,7637, 7638,7639, 7640,7641, 7642,7436, 7435 & 
& ,7643, 7644,7645, 7646,7647, 7648], & 
& edgecnc=[1142,1247,1248,1249,1146,1250,1251,1252], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(169),elname="xbrick",eltype="xbrick",typekey=169) 

        call prepare(lib_xbrick(170),key=170, & 
& nodecnc=[1601,421,420,1600,4173,2993,2992,4172,7649, 7650,7651, 7652,7412, 7411,7640, 7639,7653, 7654 & 
& ,7655, 7656,7420, 7419,7646, 7645], & 
& edgecnc=[1253,1254,1134,1248,1255,1256,1138,1251], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(170),elname="xbrick",eltype="xbrick",typekey=170) 

        call prepare(lib_xbrick(171),key=171, & 
& nodecnc=[1506,53,426,1509,4078,2625,2998,4081,7657, 7658,7659, 7660,7661, 7662,7663, 7664,7665, 7666 & 
& ,7667, 7668,7669, 7670,7671, 7672], & 
& edgecnc=[1257,1258,1259,1260,1261,1262,1263,1264], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(171),elname="xbrick",eltype="xbrick",typekey=171) 

        call prepare(lib_xbrick(172),key=172, & 
& nodecnc=[1505,1676,258,1509,4077,4248,2830,4081,7598, 7597,7673, 7674,7675, 7676,7677, 7678,7606, 7605 & 
& ,7679, 7680,7681, 7682,7683, 7684], & 
& edgecnc=[1227,1265,1266,1267,1231,1268,1269,1270], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(172),elname="xbrick",eltype="xbrick",typekey=172) 

        call prepare(lib_xbrick(173),key=173, & 
& nodecnc=[1827,2300,1897,1738,4399,4872,4469,4310,7685, 7686,7687, 7688,7462, 7461,7689, 7690,7691, 7692 & 
& ,7693, 7694,7470, 7469,7695, 7696], & 
& edgecnc=[1271,1272,1159,1273,1274,1275,1163,1276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(173),elname="xbrick",eltype="xbrick",typekey=173) 

        call prepare(lib_xbrick(174),key=174, & 
& nodecnc=[1827,91,1677,1764,4399,2663,4249,4336,7697, 7698,7699, 7700,7612, 7611,7701, 7702,7703, 7704 & 
& ,7705, 7706,7620, 7619,7707, 7708], & 
& edgecnc=[1277,1278,1234,1279,1280,1281,1238,1282], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(174),elname="xbrick",eltype="xbrick",typekey=174) 

        call prepare(lib_xbrick(175),key=175, & 
& nodecnc=[1511,428,427,1510,4083,3000,2999,4082,7709, 7710,7711, 7712,7713, 7714,7715, 7716,7717, 7718 & 
& ,7719, 7720,7721, 7722,7723, 7724], & 
& edgecnc=[1283,1284,1285,1286,1287,1288,1289,1290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(175),elname="xbrick",eltype="xbrick",typekey=175) 

        call prepare(lib_xbrick(176),key=176, & 
& nodecnc=[91,1680,1511,1510,2663,4252,4083,4082,7725, 7726,7727, 7728,7716, 7715,7729, 7730,7731, 7732 & 
& ,7733, 7734,7724, 7723,7735, 7736], & 
& edgecnc=[1291,1292,1286,1293,1294,1295,1290,1296], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(176),elname="xbrick",eltype="xbrick",typekey=176) 

        call prepare(lib_xbrick(177),key=177, & 
& nodecnc=[428,1511,1530,54,3000,4083,4102,2626,7710, 7709,7737, 7738,7739, 7740,7741, 7742,7718, 7717 & 
& ,7743, 7744,7745, 7746,7747, 7748], & 
& edgecnc=[1283,1297,1298,1299,1287,1300,1301,1302], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(177),elname="xbrick",eltype="xbrick",typekey=177) 

        call prepare(lib_xbrick(178),key=178, & 
& nodecnc=[1563,431,55,1531,4135,3003,2627,4103,7749, 7750,7751, 7752,7753, 7754,7755, 7756,7757, 7758 & 
& ,7759, 7760,7761, 7762,7763, 7764], & 
& edgecnc=[1303,1304,1305,1306,1307,1308,1309,1310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(178),elname="xbrick",eltype="xbrick",typekey=178) 

        call prepare(lib_xbrick(179),key=179, & 
& nodecnc=[1530,93,2108,1561,4102,2665,4680,4133,7765, 7766,7767, 7768,7769, 7770,7771, 7772,7773, 7774 & 
& ,7775, 7776,7777, 7778,7779, 7780], & 
& edgecnc=[1311,1312,1313,1314,1315,1316,1317,1318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(179),elname="xbrick",eltype="xbrick",typekey=179) 

        call prepare(lib_xbrick(180),key=180, & 
& nodecnc=[1563,2220,124,1564,4135,4792,2696,4136,7781, 7782,7783, 7784,7785, 7786,7787, 7788,7789, 7790 & 
& ,7791, 7792,7793, 7794,7795, 7796], & 
& edgecnc=[1319,1320,1321,1322,1323,1324,1325,1326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(180),elname="xbrick",eltype="xbrick",typekey=180) 

        call prepare(lib_xbrick(181),key=181, & 
& nodecnc=[124,1687,1532,1564,2696,4259,4104,4136,7797, 7798,7799, 7800,7801, 7802,7786, 7785,7803, 7804 & 
& ,7805, 7806,7807, 7808,7794, 7793], & 
& edgecnc=[1327,1328,1329,1321,1330,1331,1332,1325], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(181),elname="xbrick",eltype="xbrick",typekey=181) 

        call prepare(lib_xbrick(182),key=182, & 
& nodecnc=[292,1684,1533,1565,2864,4256,4105,4137,7809, 7810,7811, 7812,7813, 7814,7815, 7816,7817, 7818 & 
& ,7819, 7820,7821, 7822,7823, 7824], & 
& edgecnc=[1333,1334,1335,1336,1337,1338,1339,1340], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(182),elname="xbrick",eltype="xbrick",typekey=182) 

        call prepare(lib_xbrick(183),key=183, & 
& nodecnc=[1687,292,1565,1532,4259,2864,4137,4104,7508, 7507,7816, 7815,7825, 7826,7800, 7799,7516, 7515 & 
& ,7824, 7823,7827, 7828,7806, 7805], & 
& edgecnc=[1182,1336,1341,1328,1186,1340,1342,1331], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(183),elname="xbrick",eltype="xbrick",typekey=183) 

        call prepare(lib_xbrick(184),key=184, & 
& nodecnc=[1673,1912,1568,1567,4245,4484,4140,4139,7829, 7830,7831, 7832,7833, 7834,7008, 7007,7835, 7836 & 
& ,7837, 7838,7839, 7840,7016, 7015], & 
& edgecnc=[1343,1344,1345,932,1346,1347,1348,936], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(184),elname="xbrick",eltype="xbrick",typekey=184) 

        call prepare(lib_xbrick(185),key=185, & 
& nodecnc=[2004,1924,1870,294,4576,4496,4442,2866,7841, 7842,7843, 7844,7024, 7023,7845, 7846,7847, 7848 & 
& ,7849, 7850,7032, 7031,7851, 7852], & 
& edgecnc=[1349,1350,940,1351,1352,1353,944,1354], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(185),elname="xbrick",eltype="xbrick",typekey=185) 

        call prepare(lib_xbrick(186),key=186, & 
& nodecnc=[2047,2004,2095,295,4619,4576,4667,2867,7853, 7854,7855, 7856,7857, 7858,7859, 7860,7861, 7862 & 
& ,7863, 7864,7865, 7866,7867, 7868], & 
& edgecnc=[1355,1356,1357,1358,1359,1360,1361,1362], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(186),elname="xbrick",eltype="xbrick",typekey=186) 

        call prepare(lib_xbrick(187),key=187, & 
& nodecnc=[127,2168,2190,2172,2699,4740,4762,4744,7869, 7870,7871, 7872,7873, 7874,7875, 7876,7877, 7878 & 
& ,7879, 7880,7881, 7882,7883, 7884], & 
& edgecnc=[1363,1364,1365,1366,1367,1368,1369,1370], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(187),elname="xbrick",eltype="xbrick",typekey=187) 

        call prepare(lib_xbrick(188),key=188, & 
& nodecnc=[2178,2191,2201,186,4750,4763,4773,2758,7885, 7886,7887, 7888,7889, 7890,7891, 7892,7893, 7894 & 
& ,7895, 7896,7897, 7898,7899, 7900], & 
& edgecnc=[1371,1372,1373,1374,1375,1376,1377,1378], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(188),elname="xbrick",eltype="xbrick",typekey=188) 

        call prepare(lib_xbrick(189),key=189, & 
& nodecnc=[2324,220,2150,2171,4896,2792,4722,4743,7901, 7902,7903, 7904,7905, 7906,7907, 7908,7909, 7910 & 
& ,7911, 7912,7913, 7914,7915, 7916], & 
& edgecnc=[1379,1380,1381,1382,1383,1384,1385,1386], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(189),elname="xbrick",eltype="xbrick",typekey=189) 

        call prepare(lib_xbrick(190),key=190, & 
& nodecnc=[1954,2031,2082,2131,4526,4603,4654,4703,7917, 7918,7919, 7920,7921, 7922,7923, 7924,7925, 7926 & 
& ,7927, 7928,7929, 7930,7931, 7932], & 
& edgecnc=[1387,1388,1389,1390,1391,1392,1393,1394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(190),elname="xbrick",eltype="xbrick",typekey=190) 

        call prepare(lib_xbrick(191),key=191, & 
& nodecnc=[1682,1744,1972,228,4254,4316,4544,2800,7933, 7934,7935, 7936,7937, 7938,7939, 7940,7941, 7942 & 
& ,7943, 7944,7945, 7946,7947, 7948], & 
& edgecnc=[1395,1396,1397,1398,1399,1400,1401,1402], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(191),elname="xbrick",eltype="xbrick",typekey=191) 

        call prepare(lib_xbrick(192),key=192, & 
& nodecnc=[2250,1972,1744,1889,4822,4544,4316,4461,7949, 7950,7936, 7935,7951, 7952,7953, 7954,7955, 7956 & 
& ,7944, 7943,7957, 7958,7959, 7960], & 
& edgecnc=[1403,1396,1404,1405,1406,1400,1407,1408], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(192),elname="xbrick",eltype="xbrick",typekey=192) 

        call prepare(lib_xbrick(193),key=193, & 
& nodecnc=[1666,2153,1877,183,4238,4725,4449,2755,7961, 7962,7963, 7964,7965, 7966,7967, 7968,7969, 7970 & 
& ,7971, 7972,7973, 7974,7975, 7976], & 
& edgecnc=[1409,1410,1411,1412,1413,1414,1415,1416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(193),elname="xbrick",eltype="xbrick",typekey=193) 

        call prepare(lib_xbrick(194),key=194, & 
& nodecnc=[1986,1877,2153,2041,4558,4449,4725,4613,7977, 7978,7964, 7963,7979, 7980,7981, 7982,7983, 7984 & 
& ,7972, 7971,7985, 7986,7987, 7988], & 
& edgecnc=[1417,1410,1418,1419,1420,1414,1421,1422], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(194),elname="xbrick",eltype="xbrick",typekey=194) 

        call prepare(lib_xbrick(195),key=195, & 
& nodecnc=[2141,2525,2541,2135,4713,5097,5113,4707,7989, 7990,7991, 7992,7993, 7994,7995, 7996,7997, 7998 & 
& ,7999, 8000,8001, 8002,8003, 8004], & 
& edgecnc=[1423,1424,1425,1426,1427,1428,1429,1430], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(195),elname="xbrick",eltype="xbrick",typekey=195) 

        call prepare(lib_xbrick(196),key=196, & 
& nodecnc=[2376,298,1942,2022,4948,2870,4514,4594,8005, 8006,8007, 8008,8009, 8010,8011, 8012,8013, 8014 & 
& ,8015, 8016,8017, 8018,8019, 8020], & 
& edgecnc=[1431,1432,1433,1434,1435,1436,1437,1438], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(196),elname="xbrick",eltype="xbrick",typekey=196) 

        call prepare(lib_xbrick(197),key=197, & 
& nodecnc=[1837,1779,1727,331,4409,4351,4299,2903,8021, 8022,8023, 8024,8025, 8026,8027, 8028,8029, 8030 & 
& ,8031, 8032,8033, 8034,8035, 8036], & 
& edgecnc=[1439,1440,1441,1442,1443,1444,1445,1446], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(197),elname="xbrick",eltype="xbrick",typekey=197) 

        call prepare(lib_xbrick(198),key=198, & 
& nodecnc=[1546,1534,445,444,4118,4106,3017,3016,8037, 8038,8039, 8040,8041, 8042,8043, 8044,8045, 8046 & 
& ,8047, 8048,8049, 8050,8051, 8052], & 
& edgecnc=[1447,1448,1449,1450,1451,1452,1453,1454], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(198),elname="xbrick",eltype="xbrick",typekey=198) 

        call prepare(lib_xbrick(199),key=199, & 
& nodecnc=[329,2065,1570,1569,2901,4637,4142,4141,8053, 8054,8055, 8056,8057, 8058,8059, 8060,8061, 8062 & 
& ,8063, 8064,8065, 8066,8067, 8068], & 
& edgecnc=[1455,1456,1457,1458,1459,1460,1461,1462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(199),elname="xbrick",eltype="xbrick",typekey=199) 

        call prepare(lib_xbrick(200),key=200, & 
& nodecnc=[1535,1569,443,442,4107,4141,3015,3014,8069, 8070,8071, 8072,8073, 8074,8075, 8076,8077, 8078 & 
& ,8079, 8080,8081, 8082,8083, 8084], & 
& edgecnc=[1463,1464,1465,1466,1467,1468,1469,1470], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(200),elname="xbrick",eltype="xbrick",typekey=200) 

        call prepare(lib_xbrick(201),key=201, & 
& nodecnc=[1571,1536,446,63,4143,4108,3018,2635,8085, 8086,8087, 8088,8089, 8090,8091, 8092,8093, 8094 & 
& ,8095, 8096,8097, 8098,8099, 8100], & 
& edgecnc=[1471,1472,1473,1474,1475,1476,1477,1478], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(201),elname="xbrick",eltype="xbrick",typekey=201) 

        call prepare(lib_xbrick(202),key=202, & 
& nodecnc=[332,1572,1536,1727,2904,4144,4108,4299,8101, 8102,8103, 8104,8105, 8106,8107, 8108,8109, 8110 & 
& ,8111, 8112,8113, 8114,8115, 8116], & 
& edgecnc=[1479,1480,1481,1482,1483,1484,1485,1486], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(202),elname="xbrick",eltype="xbrick",typekey=202) 

        call prepare(lib_xbrick(203),key=203, & 
& nodecnc=[333,1537,1573,1693,2905,4109,4145,4265,8117, 8118,8119, 8120,8121, 8122,8123, 8124,8125, 8126 & 
& ,8127, 8128,8129, 8130,8131, 8132], & 
& edgecnc=[1487,1488,1489,1490,1491,1492,1493,1494], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(203),elname="xbrick",eltype="xbrick",typekey=203) 

        call prepare(lib_xbrick(204),key=204, & 
& nodecnc=[1574,1691,334,1538,4146,4263,2906,4110,8133, 8134,8135, 8136,8137, 8138,8139, 8140,8141, 8142 & 
& ,8143, 8144,8145, 8146,8147, 8148], & 
& edgecnc=[1495,1496,1497,1498,1499,1500,1501,1502], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(204),elname="xbrick",eltype="xbrick",typekey=204) 

        call prepare(lib_xbrick(205),key=205, & 
& nodecnc=[1539,1576,452,451,4111,4148,3024,3023,6716, 6715,8149, 8150,8151, 8152,8153, 8154,6724, 6723 & 
& ,8155, 8156,8157, 8158,8159, 8160], & 
& edgecnc=[786,1503,1504,1505,790,1506,1507,1508], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(205),elname="xbrick",eltype="xbrick",typekey=205) 

        call prepare(lib_xbrick(206),key=206, & 
& nodecnc=[450,449,1538,1540,3022,3021,4110,4112,8161, 8162,8163, 8164,8165, 8166,8167, 8168,8169, 8170 & 
& ,8171, 8172,8173, 8174,8175, 8176], & 
& edgecnc=[1509,1510,1511,1512,1513,1514,1515,1516], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(206),elname="xbrick",eltype="xbrick",typekey=206) 

        call prepare(lib_xbrick(207),key=207, & 
& nodecnc=[1848,1777,1723,334,4420,4349,4295,2906,8177, 8178,8179, 8180,8181, 8182,8183, 8184,8185, 8186 & 
& ,8187, 8188,8189, 8190,8191, 8192], & 
& edgecnc=[1517,1518,1519,1520,1521,1522,1523,1524], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(207),elname="xbrick",eltype="xbrick",typekey=207) 

        call prepare(lib_xbrick(208),key=208, & 
& nodecnc=[1834,335,1723,1777,4406,2907,4295,4349,8193, 8194,8195, 8196,8180, 8179,8197, 8198,8199, 8200 & 
& ,8201, 8202,8188, 8187,8203, 8204], & 
& edgecnc=[1525,1526,1518,1527,1528,1529,1522,1530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(208),elname="xbrick",eltype="xbrick",typekey=208) 

        call prepare(lib_xbrick(209),key=209, & 
& nodecnc=[1576,1798,336,1577,4148,4370,2908,4149,6714, 6713,8205, 8206,8207, 8208,8209, 8210,6722, 6721 & 
& ,8211, 8212,8213, 8214,8215, 8216], & 
& edgecnc=[785,1531,1532,1533,789,1534,1535,1536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(209),elname="xbrick",eltype="xbrick",typekey=209) 

        call prepare(lib_xbrick(210),key=210, & 
& nodecnc=[454,453,1541,1578,3026,3025,4113,4150,8217, 8218,8219, 8220,8221, 8222,8223, 8224,8225, 8226 & 
& ,8227, 8228,8229, 8230,8231, 8232], & 
& edgecnc=[1537,1538,1539,1540,1541,1542,1543,1544], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(210),elname="xbrick",eltype="xbrick",typekey=210) 

        call prepare(lib_xbrick(211),key=211, & 
& nodecnc=[1260,1748,1579,1729,3832,4320,4151,4301,8233, 8234,8235, 8236,8237, 8238,8239, 8240,8241, 8242 & 
& ,8243, 8244,8245, 8246,8247, 8248], & 
& edgecnc=[1545,1546,1547,1548,1549,1550,1551,1552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(211),elname="xbrick",eltype="xbrick",typekey=211) 

        call prepare(lib_xbrick(212),key=212, & 
& nodecnc=[1579,1471,457,69,4151,4043,3029,2641,8249, 8250,8251, 8252,8253, 8254,8255, 8256,8257, 8258 & 
& ,8259, 8260,8261, 8262,8263, 8264], & 
& edgecnc=[1553,1554,1555,1556,1557,1558,1559,1560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(212),elname="xbrick",eltype="xbrick",typekey=212) 

        call prepare(lib_xbrick(213),key=213, & 
& nodecnc=[1166,1096,1064,305,3738,3668,3636,2877,8265, 8266,8267, 8268,8269, 8270,8271, 8272,8273, 8274 & 
& ,8275, 8276,8277, 8278,8279, 8280], & 
& edgecnc=[1561,1562,1563,1564,1565,1566,1567,1568], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(213),elname="xbrick",eltype="xbrick",typekey=213) 

        call prepare(lib_xbrick(214),key=214, & 
& nodecnc=[1121,1095,1039,1064,3693,3667,3611,3636,8281, 8282,8283, 8284,8285, 8286,8287, 8288,8289, 8290 & 
& ,8291, 8292,8293, 8294,8295, 8296], & 
& edgecnc=[1569,1570,1571,1572,1573,1574,1575,1576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(214),elname="xbrick",eltype="xbrick",typekey=214) 

        call prepare(lib_xbrick(215),key=215, & 
& nodecnc=[942,966,995,2438,3514,3538,3567,5010,8297, 8298,8299, 8300,8301, 8302,8303, 8304,8305, 8306 & 
& ,8307, 8308,8309, 8310,8311, 8312], & 
& edgecnc=[1577,1578,1579,1580,1581,1582,1583,1584], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(215),elname="xbrick",eltype="xbrick",typekey=215) 

        call prepare(lib_xbrick(216),key=216, & 
& nodecnc=[882,2538,816,849,3454,5110,3388,3421,8313, 8314,8315, 8316,8317, 8318,8319, 8320,8321, 8322 & 
& ,8323, 8324,8325, 8326,8327, 8328], & 
& edgecnc=[1585,1586,1587,1588,1589,1590,1591,1592], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(216),elname="xbrick",eltype="xbrick",typekey=216) 

        call prepare(lib_xbrick(217),key=217, & 
& nodecnc=[754,718,646,692,3326,3290,3218,3264,8329, 8330,8331, 8332,8333, 8334,8335, 8336,8337, 8338 & 
& ,8339, 8340,8341, 8342,8343, 8344], & 
& edgecnc=[1593,1594,1595,1596,1597,1598,1599,1600], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(217),elname="xbrick",eltype="xbrick",typekey=217) 

        call prepare(lib_xbrick(218),key=218, & 
& nodecnc=[2424,208,2461,2454,4996,2780,5033,5026,8345, 8346,8347, 8348,8349, 8350,8351, 8352,8353, 8354 & 
& ,8355, 8356,8357, 8358,8359, 8360], & 
& edgecnc=[1601,1602,1603,1604,1605,1606,1607,1608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(218),elname="xbrick",eltype="xbrick",typekey=218) 

        call prepare(lib_xbrick(219),key=219, & 
& nodecnc=[999,274,1030,2280,3571,2846,3602,4852,8361, 8362,8363, 8364,8365, 8366,8367, 8368,8369, 8370 & 
& ,8371, 8372,8373, 8374,8375, 8376], & 
& edgecnc=[1609,1610,1611,1612,1613,1614,1615,1616], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(219),elname="xbrick",eltype="xbrick",typekey=219) 

        call prepare(lib_xbrick(220),key=220, & 
& nodecnc=[1466,1656,1204,1472,4038,4228,3776,4044,8377, 8378,8379, 8380,8381, 8382,6652, 6651,8383, 8384 & 
& ,8385, 8386,8387, 8388,6660, 6659], & 
& edgecnc=[1617,1618,1619,754,1620,1621,1622,758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(220),elname="xbrick",eltype="xbrick",typekey=220) 

        call prepare(lib_xbrick(221),key=221, & 
& nodecnc=[401,38,1203,1229,2973,2610,3775,3801,8389, 8390,8391, 8392,8393, 8394,8395, 8396,8397, 8398 & 
& ,8399, 8400,8401, 8402,8403, 8404], & 
& edgecnc=[1623,1624,1625,1626,1627,1628,1629,1630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(221),elname="xbrick",eltype="xbrick",typekey=221) 

        call prepare(lib_xbrick(222),key=222, & 
& nodecnc=[239,1171,1203,1186,2811,3743,3775,3758,8405, 8406,8407, 8408,8409, 8410,8411, 8412,8413, 8414 & 
& ,8415, 8416,8417, 8418,8419, 8420], & 
& edgecnc=[1631,1632,1633,1634,1635,1636,1637,1638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(222),elname="xbrick",eltype="xbrick",typekey=222) 

        call prepare(lib_xbrick(223),key=223, & 
& nodecnc=[237,1382,1622,1392,2809,3954,4194,3964,8421, 8422,8423, 8424,8425, 8426,8427, 8428,8429, 8430 & 
& ,8431, 8432,8433, 8434,8435, 8436], & 
& edgecnc=[1639,1640,1641,1642,1643,1644,1645,1646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(223),elname="xbrick",eltype="xbrick",typekey=223) 

        call prepare(lib_xbrick(224),key=224, & 
& nodecnc=[1164,1090,1141,1149,3736,3662,3713,3721,8437, 8438,8439, 8440,8441, 8442,8443, 8444,8445, 8446 & 
& ,8447, 8448,8449, 8450,8451, 8452], & 
& edgecnc=[1647,1648,1649,1650,1651,1652,1653,1654], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(224),elname="xbrick",eltype="xbrick",typekey=224) 

        call prepare(lib_xbrick(225),key=225, & 
& nodecnc=[209,1005,2443,1026,2781,3577,5015,3598,8453, 8454,8455, 8456,8457, 8458,8459, 8460,8461, 8462 & 
& ,8463, 8464,8465, 8466,8467, 8468], & 
& edgecnc=[1655,1656,1657,1658,1659,1660,1661,1662], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(225),elname="xbrick",eltype="xbrick",typekey=225) 

        call prepare(lib_xbrick(226),key=226, & 
& nodecnc=[1060,1079,1085,176,3632,3651,3657,2748,8469, 8470,8471, 8472,8473, 8474,8475, 8476,8477, 8478 & 
& ,8479, 8480,8481, 8482,8483, 8484], & 
& edgecnc=[1663,1664,1665,1666,1667,1668,1669,1670], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(226),elname="xbrick",eltype="xbrick",typekey=226) 

        call prepare(lib_xbrick(227),key=227, & 
& nodecnc=[2562,1058,1102,1060,5134,3630,3674,3632,8485, 8486,8487, 8488,8489, 8490,8491, 8492,8493, 8494 & 
& ,8495, 8496,8497, 8498,8499, 8500], & 
& edgecnc=[1671,1672,1673,1674,1675,1676,1677,1678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(227),elname="xbrick",eltype="xbrick",typekey=227) 

        call prepare(lib_xbrick(228),key=228, & 
& nodecnc=[2477,1082,1066,1046,5049,3654,3638,3618,8501, 8502,8503, 8504,8505, 8506,8507, 8508,8509, 8510 & 
& ,8511, 8512,8513, 8514,8515, 8516], & 
& edgecnc=[1679,1680,1681,1682,1683,1684,1685,1686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(228),elname="xbrick",eltype="xbrick",typekey=228) 

        call prepare(lib_xbrick(229),key=229, & 
& nodecnc=[1621,1150,1167,1184,4193,3722,3739,3756,8517, 8518,8519, 8520,8521, 8522,8523, 8524,8525, 8526 & 
& ,8527, 8528,8529, 8530,8531, 8532], & 
& edgecnc=[1687,1688,1689,1690,1691,1692,1693,1694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(229),elname="xbrick",eltype="xbrick",typekey=229) 

        call prepare(lib_xbrick(230),key=230, & 
& nodecnc=[1248,1227,1168,1240,3820,3799,3740,3812,8533, 8534,8535, 8536,8537, 8538,8539, 8540,8541, 8542 & 
& ,8543, 8544,8545, 8546,8547, 8548], & 
& edgecnc=[1695,1696,1697,1698,1699,1700,1701,1702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(230),elname="xbrick",eltype="xbrick",typekey=230) 

        call prepare(lib_xbrick(231),key=231, & 
& nodecnc=[1420,1268,1636,1429,3992,3840,4208,4001,8549, 8550,8551, 8552,8553, 8554,8555, 8556,8557, 8558 & 
& ,8559, 8560,8561, 8562,8563, 8564], & 
& edgecnc=[1703,1704,1705,1706,1707,1708,1709,1710], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(231),elname="xbrick",eltype="xbrick",typekey=231) 

        call prepare(lib_xbrick(232),key=232, & 
& nodecnc=[1748,1811,1471,1579,4320,4383,4043,4151,8565, 8566,8567, 8568,8250, 8249,8236, 8235,8569, 8570 & 
& ,8571, 8572,8258, 8257,8244, 8243], & 
& edgecnc=[1711,1712,1553,1546,1713,1714,1557,1550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(232),elname="xbrick",eltype="xbrick",typekey=232) 

        call prepare(lib_xbrick(233),key=233, & 
& nodecnc=[1156,1154,1473,1705,3728,3726,4045,4277,8573, 8574,8575, 8576,8577, 8578,8579, 8580,8581, 8582 & 
& ,8583, 8584,8585, 8586,8587, 8588], & 
& edgecnc=[1715,1716,1717,1718,1719,1720,1721,1722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(233),elname="xbrick",eltype="xbrick",typekey=233) 

        call prepare(lib_xbrick(234),key=234, & 
& nodecnc=[1010,1036,984,306,3582,3608,3556,2878,8589, 8590,8591, 8592,8593, 8594,8595, 8596,8597, 8598 & 
& ,8599, 8600,8601, 8602,8603, 8604], & 
& edgecnc=[1723,1724,1725,1726,1727,1728,1729,1730], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(234),elname="xbrick",eltype="xbrick",typekey=234) 

        call prepare(lib_xbrick(235),key=235, & 
& nodecnc=[765,712,653,685,3337,3284,3225,3257,8605, 8606,8607, 8608,8609, 8610,8611, 8612,8613, 8614 & 
& ,8615, 8616,8617, 8618,8619, 8620], & 
& edgecnc=[1731,1732,1733,1734,1735,1736,1737,1738], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(235),elname="xbrick",eltype="xbrick",typekey=235) 

        call prepare(lib_xbrick(236),key=236, & 
& nodecnc=[640,612,556,576,3212,3184,3128,3148,8621, 8622,8623, 8624,8625, 8626,8627, 8628,8629, 8630 & 
& ,8631, 8632,8633, 8634,8635, 8636], & 
& edgecnc=[1739,1740,1741,1742,1743,1744,1745,1746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(236),elname="xbrick",eltype="xbrick",typekey=236) 

        call prepare(lib_xbrick(237),key=237, & 
& nodecnc=[174,608,649,579,2746,3180,3221,3151,8637, 8638,8639, 8640,8641, 8642,8643, 8644,8645, 8646 & 
& ,8647, 8648,8649, 8650,8651, 8652], & 
& edgecnc=[1747,1748,1749,1750,1751,1752,1753,1754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(237),elname="xbrick",eltype="xbrick",typekey=237) 

        call prepare(lib_xbrick(238),key=238, & 
& nodecnc=[460,459,1152,1155,3032,3031,3724,3727,8653, 8654,8655, 8656,8657, 8658,8659, 8660,8661, 8662 & 
& ,8663, 8664,8665, 8666,8667, 8668], & 
& edgecnc=[1755,1756,1757,1758,1759,1760,1761,1762], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(238),elname="xbrick",eltype="xbrick",typekey=238) 

        call prepare(lib_xbrick(239),key=239, & 
& nodecnc=[340,1111,1155,1152,2912,3683,3727,3724,8669, 8670,8671, 8672,8658, 8657,8673, 8674,8675, 8676 & 
& ,8677, 8678,8666, 8665,8679, 8680], & 
& edgecnc=[1763,1764,1757,1765,1766,1767,1761,1768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(239),elname="xbrick",eltype="xbrick",typekey=239) 

        call prepare(lib_xbrick(240),key=240, & 
& nodecnc=[1025,987,941,1092,3597,3559,3513,3664,8681, 8682,8683, 8684,8685, 8686,8687, 8688,8689, 8690 & 
& ,8691, 8692,8693, 8694,8695, 8696], & 
& edgecnc=[1769,1770,1771,1772,1773,1774,1775,1776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(240),elname="xbrick",eltype="xbrick",typekey=240) 

        call prepare(lib_xbrick(241),key=241, & 
& nodecnc=[2339,707,727,669,4911,3279,3299,3241,8697, 8698,8699, 8700,8701, 8702,8703, 8704,8705, 8706 & 
& ,8707, 8708,8709, 8710,8711, 8712], & 
& edgecnc=[1777,1778,1779,1780,1781,1782,1783,1784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(241),elname="xbrick",eltype="xbrick",typekey=241) 

        call prepare(lib_xbrick(242),key=242, & 
& nodecnc=[565,540,503,511,3137,3112,3075,3083,8713, 8714,8715, 8716,5238, 5237,8717, 8718,8719, 8720 & 
& ,8721, 8722,5244, 5243,8723, 8724], & 
& edgecnc=[1785,1786,47,1787,1788,1789,50,1790], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(242),elname="xbrick",eltype="xbrick",typekey=242) 

        call prepare(lib_xbrick(243),key=243, & 
& nodecnc=[1170,1646,1100,1120,3742,4218,3672,3692,8725, 8726,8727, 8728,8729, 8730,8731, 8732,8733, 8734 & 
& ,8735, 8736,8737, 8738,8739, 8740], & 
& edgecnc=[1791,1792,1793,1794,1795,1796,1797,1798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(243),elname="xbrick",eltype="xbrick",typekey=243) 

        call prepare(lib_xbrick(244),key=244, & 
& nodecnc=[920,1057,1034,1050,3492,3629,3606,3622,8741, 8742,8743, 8744,8745, 8746,8747, 8748,8749, 8750 & 
& ,8751, 8752,8753, 8754,8755, 8756], & 
& edgecnc=[1799,1800,1801,1802,1803,1804,1805,1806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(244),elname="xbrick",eltype="xbrick",typekey=244) 

        call prepare(lib_xbrick(245),key=245, & 
& nodecnc=[2297,735,761,695,4869,3307,3333,3267,8757, 8758,8759, 8760,8761, 8762,8763, 8764,8765, 8766 & 
& ,8767, 8768,8769, 8770,8771, 8772], & 
& edgecnc=[1807,1808,1809,1810,1811,1812,1813,1814], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(245),elname="xbrick",eltype="xbrick",typekey=245) 

        call prepare(lib_xbrick(246),key=246, & 
& nodecnc=[761,735,851,772,3333,3307,3423,3344,8760, 8759,8773, 8774,8775, 8776,8777, 8778,8768, 8767 & 
& ,8779, 8780,8781, 8782,8783, 8784], & 
& edgecnc=[1808,1815,1816,1817,1812,1818,1819,1820], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(246),elname="xbrick",eltype="xbrick",typekey=246) 

        call prepare(lib_xbrick(247),key=247, & 
& nodecnc=[73,1262,1261,465,2645,3834,3833,3037,8785, 8786,8787, 8788,8789, 8790,8791, 8792,8793, 8794 & 
& ,8795, 8796,8797, 8798,8799, 8800], & 
& edgecnc=[1821,1822,1823,1824,1825,1826,1827,1828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(247),elname="xbrick",eltype="xbrick",typekey=247) 

        call prepare(lib_xbrick(248),key=248, & 
& nodecnc=[344,1215,1223,1259,2916,3787,3795,3831,8801, 8802,8803, 8804,8805, 8806,8807, 8808,8809, 8810 & 
& ,8811, 8812,8813, 8814,8815, 8816], & 
& edgecnc=[1829,1830,1831,1832,1833,1834,1835,1836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(248),elname="xbrick",eltype="xbrick",typekey=248) 

        call prepare(lib_xbrick(249),key=249, & 
& nodecnc=[1134,1157,1176,310,3706,3729,3748,2882,8817, 8818,5560, 5559,8819, 8820,8821, 8822,8823, 8824 & 
& ,5568, 5567,8825, 8826,8827, 8828], & 
& edgecnc=[1837,208,1838,1839,1840,212,1841,1842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(249),elname="xbrick",eltype="xbrick",typekey=249) 

        call prepare(lib_xbrick(250),key=250, & 
& nodecnc=[960,1000,1022,973,3532,3572,3594,3545,8829, 8830,8831, 8832,8833, 8834,8835, 8836,8837, 8838 & 
& ,8839, 8840,8841, 8842,8843, 8844], & 
& edgecnc=[1843,1844,1845,1846,1847,1848,1849,1850], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(250),elname="xbrick",eltype="xbrick",typekey=250) 

        call prepare(lib_xbrick(251),key=251, & 
& nodecnc=[766,714,679,732,3338,3286,3251,3304,8845, 8846,8847, 8848,8849, 8850,8851, 8852,8853, 8854 & 
& ,8855, 8856,8857, 8858,8859, 8860], & 
& edgecnc=[1851,1852,1853,1854,1855,1856,1857,1858], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(251),elname="xbrick",eltype="xbrick",typekey=251) 

        call prepare(lib_xbrick(252),key=252, & 
& nodecnc=[3,518,512,360,2575,3090,3084,2932,8861, 8862,8863, 8864,8865, 8866,8867, 8868,8869, 8870,8871 & 
& , 8872,8873, 8874,8875, 8876], & 
& edgecnc=[1859,1860,1861,1862,1863,1864,1865,1866], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(252),elname="xbrick",eltype="xbrick",typekey=252) 

        call prepare(lib_xbrick(253),key=253, & 
& nodecnc=[1355,1386,2208,1369,3927,3958,4780,3941,8877, 8878,8879, 8880,8881, 8882,8883, 8884,8885, 8886 & 
& ,8887, 8888,8889, 8890,8891, 8892], & 
& edgecnc=[1867,1868,1869,1870,1871,1872,1873,1874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(253),elname="xbrick",eltype="xbrick",typekey=253) 

        call prepare(lib_xbrick(254),key=254, & 
& nodecnc=[1327,1428,1363,1342,3899,4000,3935,3914,8893, 8894,8895, 8896,8897, 8898,8899, 8900,8901, 8902 & 
& ,8903, 8904,8905, 8906,8907, 8908], & 
& edgecnc=[1875,1876,1877,1878,1879,1880,1881,1882], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(254),elname="xbrick",eltype="xbrick",typekey=254) 

        call prepare(lib_xbrick(255),key=255, & 
& nodecnc=[1643,2512,1353,2487,4215,5084,3925,5059,8909, 8910,8911, 8912,8913, 8914,8915, 8916,8917, 8918 & 
& ,8919, 8920,8921, 8922,8923, 8924], & 
& edgecnc=[1883,1884,1885,1886,1887,1888,1889,1890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(255),elname="xbrick",eltype="xbrick",typekey=255) 

        call prepare(lib_xbrick(256),key=256, & 
& nodecnc=[2061,2072,2093,2485,4633,4644,4665,5057,8925, 8926,8927, 8928,8929, 8930,8931, 8932,8933, 8934 & 
& ,8935, 8936,8937, 8938,8939, 8940], & 
& edgecnc=[1891,1892,1893,1894,1895,1896,1897,1898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(256),elname="xbrick",eltype="xbrick",typekey=256) 

        call prepare(lib_xbrick(257),key=257, & 
& nodecnc=[2485,2060,2426,2061,5057,4632,4998,4633,8941, 8942,8943, 8944,8945, 8946,8932, 8931,8947, 8948 & 
& ,8949, 8950,8951, 8952,8940, 8939], & 
& edgecnc=[1899,1900,1901,1894,1902,1903,1904,1898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(257),elname="xbrick",eltype="xbrick",typekey=257) 

        call prepare(lib_xbrick(258),key=258, & 
& nodecnc=[1849,1778,1722,248,4421,4350,4294,2820,8953, 8954,8955, 8956,8957, 8958,8959, 8960,8961, 8962 & 
& ,8963, 8964,8965, 8966,8967, 8968], & 
& edgecnc=[1905,1906,1907,1908,1909,1910,1911,1912], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(258),elname="xbrick",eltype="xbrick",typekey=258) 

        call prepare(lib_xbrick(259),key=259, & 
& nodecnc=[1835,247,1722,1778,4407,2819,4294,4350,5702, 5701,8969, 8970,8956, 8955,8971, 8972,5710, 5709 & 
& ,8973, 8974,8964, 8963,8975, 8976], & 
& edgecnc=[279,1913,1906,1914,283,1915,1910,1916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(259),elname="xbrick",eltype="xbrick",typekey=259) 

        call prepare(lib_xbrick(260),key=260, & 
& nodecnc=[2062,2504,1962,2523,4634,5076,4534,5095,8977, 8978,8979, 8980,8981, 8982,8983, 8984,8985, 8986 & 
& ,8987, 8988,8989, 8990,8991, 8992], & 
& edgecnc=[1917,1918,1919,1920,1921,1922,1923,1924], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(260),elname="xbrick",eltype="xbrick",typekey=260) 

        call prepare(lib_xbrick(261),key=261, & 
& nodecnc=[1808,1699,1832,1859,4380,4271,4404,4431,8993, 8994,8995, 8996,8997, 8998,8999, 9000,9001, 9002 & 
& ,9003, 9004,9005, 9006,9007, 9008], & 
& edgecnc=[1925,1926,1927,1928,1929,1930,1931,1932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(261),elname="xbrick",eltype="xbrick",typekey=261) 

        call prepare(lib_xbrick(262),key=262, & 
& nodecnc=[1951,2120,2345,2033,4523,4692,4917,4605,9009, 9010,9011, 9012,9013, 9014,9015, 9016,9017, 9018 & 
& ,9019, 9020,9021, 9022,9023, 9024], & 
& edgecnc=[1933,1934,1935,1936,1937,1938,1939,1940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(262),elname="xbrick",eltype="xbrick",typekey=262) 

        call prepare(lib_xbrick(263),key=263, & 
& nodecnc=[1839,356,1712,1761,4411,2928,4284,4333,5868, 5867,9025, 9026,9027, 9028,9029, 9030,5876, 5875 & 
& ,9031, 9032,9033, 9034,9035, 9036], & 
& edgecnc=[362,1941,1942,1943,366,1944,1945,1946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(263),elname="xbrick",eltype="xbrick",typekey=263) 

        call prepare(lib_xbrick(264),key=264, & 
& nodecnc=[1712,356,1608,1607,4284,2928,4180,4179,9026, 9025,9037, 9038,9039, 9040,9041, 9042,9032, 9031 & 
& ,9043, 9044,9045, 9046,9047, 9048], & 
& edgecnc=[1941,1947,1948,1949,1944,1950,1951,1952], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(264),elname="xbrick",eltype="xbrick",typekey=264) 

        call prepare(lib_xbrick(265),key=265, & 
& nodecnc=[83,1591,1609,484,2655,4163,4181,3056,9049, 9050,9051, 9052,9053, 9054,9055, 9056,9057, 9058 & 
& ,9059, 9060,9061, 9062,9063, 9064], & 
& edgecnc=[1953,1954,1955,1956,1957,1958,1959,1960], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(265),elname="xbrick",eltype="xbrick",typekey=265) 

        call prepare(lib_xbrick(266),key=266, & 
& nodecnc=[1637,1751,105,1452,4209,4323,2677,4024,9065, 9066,9067, 9068,9069, 9070,9071, 9072,9073, 9074 & 
& ,9075, 9076,9077, 9078,9079, 9080], & 
& edgecnc=[1961,1962,1963,1964,1965,1966,1967,1968], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(266),elname="xbrick",eltype="xbrick",typekey=266) 

        call prepare(lib_xbrick(267),key=267, & 
& nodecnc=[151,1652,1699,1702,2723,4224,4271,4274,9081, 9082,9083, 9084,9085, 9086,9087, 9088,9089, 9090 & 
& ,9091, 9092,9093, 9094,9095, 9096], & 
& edgecnc=[1969,1970,1971,1972,1973,1974,1975,1976], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(267),elname="xbrick",eltype="xbrick",typekey=267) 

        call prepare(lib_xbrick(268),key=268, & 
& nodecnc=[152,1888,1928,1847,2724,4460,4500,4419,9097, 9098,9099, 9100,9101, 9102,9103, 9104,9105, 9106 & 
& ,9107, 9108,9109, 9110,9111, 9112], & 
& edgecnc=[1977,1978,1979,1980,1981,1982,1983,1984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(268),elname="xbrick",eltype="xbrick",typekey=268) 

        call prepare(lib_xbrick(269),key=269, & 
& nodecnc=[322,1824,1741,1892,2894,4396,4313,4464,9113, 9114,9115, 9116,9117, 9118,9119, 9120,9121, 9122 & 
& ,9123, 9124,9125, 9126,9127, 9128], & 
& edgecnc=[1985,1986,1987,1988,1989,1990,1991,1992], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(269),elname="xbrick",eltype="xbrick",typekey=269) 

        call prepare(lib_xbrick(270),key=270, & 
& nodecnc=[1592,1607,482,82,4164,4179,3054,2654,9129, 9130,9131, 9132,9133, 9134,9135, 9136,9137, 9138 & 
& ,9139, 9140,9141, 9142,9143, 9144], & 
& edgecnc=[1993,1994,1995,1996,1997,1998,1999,2000], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(270),elname="xbrick",eltype="xbrick",typekey=270) 

        call prepare(lib_xbrick(271),key=271, & 
& nodecnc=[1608,483,482,1607,4180,3055,3054,4179,9145, 9146,9147, 9148,9132, 9131,9040, 9039,9149, 9150 & 
& ,9151, 9152,9140, 9139,9046, 9045], & 
& edgecnc=[2001,2002,1994,1948,2003,2004,1998,1951], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(271),elname="xbrick",eltype="xbrick",typekey=271) 

        call prepare(lib_xbrick(272),key=272, & 
& nodecnc=[481,480,1593,1606,3053,3052,4165,4178,9153, 9154,9155, 9156,9157, 9158,9159, 9160,9161, 9162 & 
& ,9163, 9164,9165, 9166,9167, 9168], & 
& edgecnc=[2005,2006,2007,2008,2009,2010,2011,2012], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(272),elname="xbrick",eltype="xbrick",typekey=272) 

        call prepare(lib_xbrick(273),key=273, & 
& nodecnc=[1695,2134,1977,1741,4267,4706,4549,4313,9169, 9170,9171, 9172,9173, 9174,9175, 9176,9177, 9178 & 
& ,9179, 9180,9181, 9182,9183, 9184], & 
& edgecnc=[2013,2014,2015,2016,2017,2018,2019,2020], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(273),elname="xbrick",eltype="xbrick",typekey=273) 

        call prepare(lib_xbrick(274),key=274, & 
& nodecnc=[1984,320,1847,1928,4556,2892,4419,4500,9185, 9186,9187, 9188,9102, 9101,9189, 9190,9191, 9192 & 
& ,9193, 9194,9110, 9109,9195, 9196], & 
& edgecnc=[2021,2022,1979,2023,2024,2025,1983,2026], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(274),elname="xbrick",eltype="xbrick",typekey=274) 

        call prepare(lib_xbrick(275),key=275, & 
& nodecnc=[1450,1452,1652,151,4022,4024,4224,2723,9197, 9198,9199, 9200,9082, 9081,9201, 9202,9203, 9204 & 
& ,9205, 9206,9090, 9089,9207, 9208], & 
& edgecnc=[2027,2028,1969,2029,2030,2031,1973,2032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(275),elname="xbrick",eltype="xbrick",typekey=275) 

        call prepare(lib_xbrick(276),key=276, & 
& nodecnc=[1751,1637,104,1660,4323,4209,2676,4232,9066, 9065,9209, 9210,9211, 9212,9213, 9214,9074, 9073 & 
& ,9215, 9216,9217, 9218,9219, 9220], & 
& edgecnc=[1961,2033,2034,2035,1965,2036,2037,2038], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(276),elname="xbrick",eltype="xbrick",typekey=276) 

        call prepare(lib_xbrick(277),key=277, & 
& nodecnc=[115,1786,116,1864,2687,4358,2688,4436,9221, 9222,9223, 9224,9225, 9226,9227, 9228,9229, 9230 & 
& ,9231, 9232,9233, 9234,9235, 9236], & 
& edgecnc=[2039,2040,2041,2042,2043,2044,2045,2046], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(277),elname="xbrick",eltype="xbrick",typekey=277) 

        call prepare(lib_xbrick(278),key=278, & 
& nodecnc=[199,2067,2073,2040,2771,4639,4645,4612,9237, 9238,9239, 9240,9241, 9242,9243, 9244,9245, 9246 & 
& ,9247, 9248,9249, 9250,9251, 9252], & 
& edgecnc=[2047,2048,2049,2050,2051,2052,2053,2054], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(278),elname="xbrick",eltype="xbrick",typekey=278) 

        call prepare(lib_xbrick(279),key=279, & 
& nodecnc=[1849,248,1690,1792,4421,2820,4262,4364,8960, 8959,9253, 9254,9255, 9256,9257, 9258,8968, 8967 & 
& ,9259, 9260,9261, 9262,9263, 9264], & 
& edgecnc=[1908,2055,2056,2057,1912,2058,2059,2060], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(279),elname="xbrick",eltype="xbrick",typekey=279) 

        call prepare(lib_xbrick(280),key=280, & 
& nodecnc=[2509,2403,282,2474,5081,4975,2854,5046,9265, 9266,9267, 9268,9269, 9270,9271, 9272,9273, 9274 & 
& ,9275, 9276,9277, 9278,9279, 9280], & 
& edgecnc=[2061,2062,2063,2064,2065,2066,2067,2068], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(280),elname="xbrick",eltype="xbrick",typekey=280) 

        call prepare(lib_xbrick(281),key=281, & 
& nodecnc=[2509,2474,2493,2075,5081,5046,5065,4647,9272, 9271,9281, 9282,9283, 9284,9285, 9286,9280, 9279 & 
& ,9287, 9288,9289, 9290,9291, 9292], & 
& edgecnc=[2064,2069,2070,2071,2068,2072,2073,2074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(281),elname="xbrick",eltype="xbrick",typekey=281) 

        call prepare(lib_xbrick(282),key=282, & 
& nodecnc=[199,2043,2117,2119,2771,4615,4689,4691,9293, 9294,9295, 9296,9297, 9298,9299, 9300,9301, 9302 & 
& ,9303, 9304,9305, 9306,9307, 9308], & 
& edgecnc=[2075,2076,2077,2078,2079,2080,2081,2082], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(282),elname="xbrick",eltype="xbrick",typekey=282) 

        call prepare(lib_xbrick(283),key=283, & 
& nodecnc=[1786,1710,1833,116,4358,4282,4405,2688,9309, 9310,9311, 9312,9313, 9314,9224, 9223,9315, 9316 & 
& ,9317, 9318,9319, 9320,9232, 9231], & 
& edgecnc=[2083,2084,2085,2040,2086,2087,2088,2044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(283),elname="xbrick",eltype="xbrick",typekey=283) 

        call prepare(lib_xbrick(284),key=284, & 
& nodecnc=[1439,1721,2451,1442,4011,4293,5023,4014,9321, 9322,9323, 9324,9325, 9326,9327, 9328,9329, 9330 & 
& ,9331, 9332,9333, 9334,9335, 9336], & 
& edgecnc=[2089,2090,2091,2092,2093,2094,2095,2096], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(284),elname="xbrick",eltype="xbrick",typekey=284) 

        call prepare(lib_xbrick(285),key=285, & 
& nodecnc=[1450,151,1629,2428,4022,2723,4201,5000,9202, 9201,9337, 9338,9339, 9340,9341, 9342,9208, 9207 & 
& ,9343, 9344,9345, 9346,9347, 9348], & 
& edgecnc=[2029,2097,2098,2099,2032,2100,2101,2102], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(285),elname="xbrick",eltype="xbrick",typekey=285) 

        call prepare(lib_xbrick(286),key=286, & 
& nodecnc=[1860,320,1872,1985,4432,2892,4444,4557,9349, 9350,9351, 9352,9353, 9354,9355, 9356,9357, 9358 & 
& ,9359, 9360,9361, 9362,9363, 9364], & 
& edgecnc=[2103,2104,2105,2106,2107,2108,2109,2110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(286),elname="xbrick",eltype="xbrick",typekey=286) 

        call prepare(lib_xbrick(287),key=287, & 
& nodecnc=[354,2111,1605,1604,2926,4683,4177,4176,9365, 9366,9367, 9368,9369, 9370,9371, 9372,9373, 9374 & 
& ,9375, 9376,9377, 9378,9379, 9380], & 
& edgecnc=[2111,2112,2113,2114,2115,2116,2117,2118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(287),elname="xbrick",eltype="xbrick",typekey=287) 

        call prepare(lib_xbrick(288),key=288, & 
& nodecnc=[2111,354,1994,2018,4683,2926,4566,4590,9366, 9365,9381, 9382,9383, 9384,9385, 9386,9374, 9373 & 
& ,9387, 9388,9389, 9390,9391, 9392], & 
& edgecnc=[2111,2119,2120,2121,2115,2122,2123,2124], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(288),elname="xbrick",eltype="xbrick",typekey=288) 

        call prepare(lib_xbrick(289),key=289, & 
& nodecnc=[1984,2063,2111,2018,4556,4635,4683,4590,9393, 9394,9395, 9396,9386, 9385,9397, 9398,9399, 9400 & 
& ,9401, 9402,9392, 9391,9403, 9404], & 
& edgecnc=[2125,2126,2121,2127,2128,2129,2124,2130], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(289),elname="xbrick",eltype="xbrick",typekey=289) 

        call prepare(lib_xbrick(290),key=290, & 
& nodecnc=[478,80,1500,1594,3050,2652,4072,4166,9405, 9406,9407, 9408,9409, 9410,9411, 9412,9413, 9414 & 
& ,9415, 9416,9417, 9418,9419, 9420], & 
& edgecnc=[2131,2132,2133,2134,2135,2136,2137,2138], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(290),elname="xbrick",eltype="xbrick",typekey=290) 

        call prepare(lib_xbrick(291),key=291, & 
& nodecnc=[353,1683,1594,1500,2925,4255,4166,4072,9421, 9422,9423, 9424,9410, 9409,9425, 9426,9427, 9428 & 
& ,9429, 9430,9418, 9417,9431, 9432], & 
& edgecnc=[2139,2140,2133,2141,2142,2143,2137,2144], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(291),elname="xbrick",eltype="xbrick",typekey=291) 

        call prepare(lib_xbrick(292),key=292, & 
& nodecnc=[1474,1499,477,476,4046,4071,3049,3048,9433, 9434,9435, 9436,9437, 9438,9439, 9440,9441, 9442 & 
& ,9443, 9444,9445, 9446,9447, 9448], & 
& edgecnc=[2145,2146,2147,2148,2149,2150,2151,2152], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(292),elname="xbrick",eltype="xbrick",typekey=292) 

        call prepare(lib_xbrick(293),key=293, & 
& nodecnc=[1437,1447,1449,317,4009,4019,4021,2889,9449, 9450,9451, 9452,9453, 9454,9455, 9456,9457, 9458 & 
& ,9459, 9460,9461, 9462,9463, 9464], & 
& edgecnc=[2153,2154,2155,2156,2157,2158,2159,2160], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(293),elname="xbrick",eltype="xbrick",typekey=293) 

        call prepare(lib_xbrick(294),key=294, & 
& nodecnc=[1422,1426,1433,148,3994,3998,4005,2720,9465, 9466,9467, 9468,9469, 9470,9471, 9472,9473, 9474 & 
& ,9475, 9476,9477, 9478,9479, 9480], & 
& edgecnc=[2161,2162,2163,2164,2165,2166,2167,2168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(294),elname="xbrick",eltype="xbrick",typekey=294) 

        call prepare(lib_xbrick(295),key=295, & 
& nodecnc=[1395,147,1373,1384,3967,2719,3945,3956,9481, 9482,9483, 9484,9485, 9486,9487, 9488,9489, 9490 & 
& ,9491, 9492,9493, 9494,9495, 9496], & 
& edgecnc=[2169,2170,2171,2172,2173,2174,2175,2176], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(295),elname="xbrick",eltype="xbrick",typekey=295) 

        call prepare(lib_xbrick(296),key=296, & 
& nodecnc=[315,1431,1395,1410,2887,4003,3967,3982,9497, 9498,9499, 9500,9501, 9502,9503, 9504,9505, 9506 & 
& ,9507, 9508,9509, 9510,9511, 9512], & 
& edgecnc=[2177,2178,2179,2180,2181,2182,2183,2184], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(296),elname="xbrick",eltype="xbrick",typekey=296) 

        call prepare(lib_xbrick(297),key=297, & 
& nodecnc=[2332,2348,2298,1413,4904,4920,4870,3985,9513, 9514,9515, 9516,9517, 9518,9519, 9520,9521, 9522 & 
& ,9523, 9524,9525, 9526,9527, 9528], & 
& edgecnc=[2185,2186,2187,2188,2189,2190,2191,2192], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(297),elname="xbrick",eltype="xbrick",typekey=297) 

        call prepare(lib_xbrick(298),key=298, & 
& nodecnc=[1475,1461,351,1460,4047,4033,2923,4032,9529, 9530,9531, 9532,9533, 9534,9535, 9536,9537, 9538 & 
& ,9539, 9540,9541, 9542,9543, 9544], & 
& edgecnc=[2193,2194,2195,2196,2197,2198,2199,2200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(298),elname="xbrick",eltype="xbrick",typekey=298) 

        call prepare(lib_xbrick(299),key=299, & 
& nodecnc=[79,475,1460,1503,2651,3047,4032,4075,9545, 9546,9547, 9548,9549, 9550,9551, 9552,9553, 9554 & 
& ,9555, 9556,9557, 9558,9559, 9560], & 
& edgecnc=[2201,2202,2203,2204,2205,2206,2207,2208], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(299),elname="xbrick",eltype="xbrick",typekey=299) 

        call prepare(lib_xbrick(300),key=300, & 
& nodecnc=[1515,490,489,1514,4087,3062,3061,4086,9561, 9562,9563, 9564,9565, 9566,9567, 9568,9569, 9570 & 
& ,9571, 9572,9573, 9574,9575, 9576], & 
& edgecnc=[2209,2210,2211,2212,2213,2214,2215,2216], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(300),elname="xbrick",eltype="xbrick",typekey=300) 

        call prepare(lib_xbrick(301),key=301, & 
& nodecnc=[156,1681,1515,1514,2728,4253,4087,4086,9577, 9578,9579, 9580,9568, 9567,9581, 9582,9583, 9584 & 
& ,9585, 9586,9576, 9575,9587, 9588], & 
& edgecnc=[2217,2218,2212,2219,2220,2221,2216,2222], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(301),elname="xbrick",eltype="xbrick",typekey=301) 

        call prepare(lib_xbrick(302),key=302, & 
& nodecnc=[2007,110,1869,1926,4579,2682,4441,4498,9589, 9590,9591, 9592,9593, 9594,9595, 9596,9597, 9598 & 
& ,9599, 9600,9601, 9602,9603, 9604], & 
& edgecnc=[2223,2224,2225,2226,2227,2228,2229,2230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(302),elname="xbrick",eltype="xbrick",typekey=302) 

        call prepare(lib_xbrick(303),key=303, & 
& nodecnc=[110,2097,2091,1946,2682,4669,4663,4518,9605, 9606,9607, 9608,9609, 9610,9611, 9612,9613, 9614 & 
& ,9615, 9616,9617, 9618,9619, 9620], & 
& edgecnc=[2231,2232,2233,2234,2235,2236,2237,2238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(303),elname="xbrick",eltype="xbrick",typekey=303) 

        call prepare(lib_xbrick(304),key=304, & 
& nodecnc=[490,1515,1542,87,3062,4087,4114,2659,9562, 9561,9621, 9622,9623, 9624,9625, 9626,9570, 9569 & 
& ,9627, 9628,9629, 9630,9631, 9632], & 
& edgecnc=[2209,2239,2240,2241,2213,2242,2243,2244], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(304),elname="xbrick",eltype="xbrick",typekey=304) 

        call prepare(lib_xbrick(305),key=305, & 
& nodecnc=[1542,92,2107,1581,4114,2664,4679,4153,9633, 9634,9635, 9636,9637, 9638,9639, 9640,9641, 9642 & 
& ,9643, 9644,9645, 9646,9647, 9648], & 
& edgecnc=[2245,2246,2247,2248,2249,2250,2251,2252], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(305),elname="xbrick",eltype="xbrick",typekey=305) 

        call prepare(lib_xbrick(306),key=306, & 
& nodecnc=[109,1869,1900,1735,2681,4441,4472,4307,9649, 9650,9651, 9652,9653, 9654,9655, 9656,9657, 9658 & 
& ,9659, 9660,9661, 9662,9663, 9664], & 
& edgecnc=[2253,2254,2255,2256,2257,2258,2259,2260], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(306),elname="xbrick",eltype="xbrick",typekey=306) 

        call prepare(lib_xbrick(307),key=307, & 
& nodecnc=[2014,1946,2091,2484,4586,4518,4663,5056,6142, 6141,9610, 9609,9665, 9666,9667, 9668,6150, 6149 & 
& ,9618, 9617,9669, 9670,9671, 9672], & 
& edgecnc=[499,2233,2261,2262,503,2237,2263,2264], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(307),elname="xbrick",eltype="xbrick",typekey=307) 

        call prepare(lib_xbrick(308),key=308, & 
& nodecnc=[1583,493,88,1543,4155,3065,2660,4115,9673, 9674,9675, 9676,9677, 9678,9679, 9680,9681, 9682 & 
& ,9683, 9684,9685, 9686,9687, 9688], & 
& edgecnc=[2265,2266,2267,2268,2269,2270,2271,2272], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(308),elname="xbrick",eltype="xbrick",typekey=308) 

        call prepare(lib_xbrick(309),key=309, & 
& nodecnc=[1946,1900,1869,110,4518,4472,4441,2682,9689, 9690,9652, 9651,9592, 9591,9612, 9611,9691, 9692 & 
& ,9660, 9659,9600, 9599,9620, 9619], & 
& edgecnc=[2273,2254,2224,2234,2274,2258,2228,2238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(309),elname="xbrick",eltype="xbrick",typekey=309) 

        call prepare(lib_xbrick(310),key=310, & 
& nodecnc=[2531,965,1019,2569,5103,3537,3591,5141,9693, 9694,9695, 9696,9697, 9698,9699, 9700,9701, 9702 & 
& ,9703, 9704,9705, 9706,9707, 9708], & 
& edgecnc=[2275,2276,2277,2278,2279,2280,2281,2282], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(310),elname="xbrick",eltype="xbrick",typekey=310) 

        call prepare(lib_xbrick(311),key=311, & 
& nodecnc=[946,870,836,837,3518,3442,3408,3409,9709, 9710,9711, 9712,9713, 9714,9715, 9716,9717, 9718 & 
& ,9719, 9720,9721, 9722,9723, 9724], & 
& edgecnc=[2283,2284,2285,2286,2287,2288,2289,2290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(311),elname="xbrick",eltype="xbrick",typekey=311) 

        call prepare(lib_xbrick(312),key=312, & 
& nodecnc=[345,1235,1302,1257,2917,3807,3874,3829,9725, 9726,9727, 9728,9729, 9730,9731, 9732,9733, 9734 & 
& ,9735, 9736,9737, 9738,9739, 9740], & 
& edgecnc=[2291,2292,2293,2294,2295,2296,2297,2298], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(312),elname="xbrick",eltype="xbrick",typekey=312) 

        call prepare(lib_xbrick(313),key=313, & 
& nodecnc=[1175,1183,143,1200,3747,3755,2715,3772,9741, 9742,9743, 9744,9745, 9746,9747, 9748,9749, 9750 & 
& ,9751, 9752,9753, 9754,9755, 9756], & 
& edgecnc=[2299,2300,2301,2302,2303,2304,2305,2306], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(313),elname="xbrick",eltype="xbrick",typekey=313) 

        call prepare(lib_xbrick(314),key=314, & 
& nodecnc=[1211,1173,1158,1190,3783,3745,3730,3762,9757, 9758,9759, 9760,9761, 9762,9763, 9764,9765, 9766 & 
& ,9767, 9768,9769, 9770,9771, 9772], & 
& edgecnc=[2307,2308,2309,2310,2311,2312,2313,2314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(314),elname="xbrick",eltype="xbrick",typekey=314) 

        call prepare(lib_xbrick(315),key=315, & 
& nodecnc=[1165,1110,120,1146,3737,3682,2692,3718,9773, 9774,9775, 9776,9777, 9778,9779, 9780,9781, 9782 & 
& ,9783, 9784,9785, 9786,9787, 9788], & 
& edgecnc=[2315,2316,2317,2318,2319,2320,2321,2322], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(315),elname="xbrick",eltype="xbrick",typekey=315) 

        call prepare(lib_xbrick(316),key=316, & 
& nodecnc=[1238,1197,1752,1228,3810,3769,4324,3800,9789, 9790,9791, 9792,9793, 9794,9795, 9796,9797, 9798 & 
& ,9799, 9800,9801, 9802,9803, 9804], & 
& edgecnc=[2323,2324,2325,2326,2327,2328,2329,2330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(316),elname="xbrick",eltype="xbrick",typekey=316) 

        call prepare(lib_xbrick(317),key=317, & 
& nodecnc=[1237,1620,1720,1615,3809,4192,4292,4187,9805, 9806,9807, 9808,9809, 9810,9811, 9812,9813, 9814 & 
& ,9815, 9816,9817, 9818,9819, 9820], & 
& edgecnc=[2331,2332,2333,2334,2335,2336,2337,2338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(317),elname="xbrick",eltype="xbrick",typekey=317) 

        call prepare(lib_xbrick(318),key=318, & 
& nodecnc=[2501,1211,2539,1220,5073,3783,5111,3792,9821, 9822,9823, 9824,9825, 9826,9827, 9828,9829, 9830 & 
& ,9831, 9832,9833, 9834,9835, 9836], & 
& edgecnc=[2339,2340,2341,2342,2343,2344,2345,2346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(318),elname="xbrick",eltype="xbrick",typekey=318) 

        call prepare(lib_xbrick(319),key=319, & 
& nodecnc=[1033,1009,969,96,3605,3581,3541,2668,6476, 6475,9837, 9838,9839, 9840,9841, 9842,6484, 6483 & 
& ,9843, 9844,9845, 9846,9847, 9848], & 
& edgecnc=[666,2347,2348,2349,670,2350,2351,2352], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(319),elname="xbrick",eltype="xbrick",typekey=319) 

        call prepare(lib_xbrick(320),key=320, & 
& nodecnc=[312,1284,2254,1286,2884,3856,4826,3858,9849, 9850,9851, 9852,9853, 9854,9855, 9856,9857, 9858 & 
& ,9859, 9860,9861, 9862,9863, 9864], & 
& edgecnc=[2353,2354,2355,2356,2357,2358,2359,2360], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(320),elname="xbrick",eltype="xbrick",typekey=320) 

        call prepare(lib_xbrick(321),key=321, & 
& nodecnc=[1266,1272,1284,1253,3838,3844,3856,3825,9865, 9866,9867, 9868,9869, 9870,9871, 9872,9873, 9874 & 
& ,9875, 9876,9877, 9878,9879, 9880], & 
& edgecnc=[2361,2362,2363,2364,2365,2366,2367,2368], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(321),elname="xbrick",eltype="xbrick",typekey=321) 

        call prepare(lib_xbrick(322),key=322, & 
& nodecnc=[144,1246,1251,1234,2716,3818,3823,3806,9881, 9882,9883, 9884,9885, 9886,9887, 9888,9889, 9890 & 
& ,9891, 9892,9893, 9894,9895, 9896], & 
& edgecnc=[2369,2370,2371,2372,2373,2374,2375,2376], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(322),elname="xbrick",eltype="xbrick",typekey=322) 

        call prepare(lib_xbrick(323),key=323, & 
& nodecnc=[1381,1367,1631,98,3953,3939,4203,2670,9897, 9898,9899, 9900,9901, 9902,9903, 9904,9905, 9906 & 
& ,9907, 9908,9909, 9910,9911, 9912], & 
& edgecnc=[2377,2378,2379,2380,2381,2382,2383,2384], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(323),elname="xbrick",eltype="xbrick",typekey=323) 

        call prepare(lib_xbrick(324),key=324, & 
& nodecnc=[119,1238,1639,1245,2691,3810,4211,3817,9913, 9914,9915, 9916,9917, 9918,9919, 9920,9921, 9922 & 
& ,9923, 9924,9925, 9926,9927, 9928], & 
& edgecnc=[2385,2386,2387,2388,2389,2390,2391,2392], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(324),elname="xbrick",eltype="xbrick",typekey=324) 

        call prepare(lib_xbrick(325),key=325, & 
& nodecnc=[1812,1806,203,1615,4384,4378,2775,4187,9929, 9930,9931, 9932,9933, 9934,9935, 9936,9937, 9938 & 
& ,9939, 9940,9941, 9942,9943, 9944], & 
& edgecnc=[2393,2394,2395,2396,2397,2398,2399,2400], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(325),elname="xbrick",eltype="xbrick",typekey=325) 

        call prepare(lib_xbrick(326),key=326, & 
& nodecnc=[279,1862,1992,1914,2851,4434,4564,4486,9945, 9946,9947, 9948,9949, 9950,9951, 9952,9953, 9954 & 
& ,9955, 9956,9957, 9958,9959, 9960], & 
& edgecnc=[2401,2402,2403,2404,2405,2406,2407,2408], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(326),elname="xbrick",eltype="xbrick",typekey=326) 

        call prepare(lib_xbrick(327),key=327, & 
& nodecnc=[1557,1527,391,33,4129,4099,2963,2605,9961, 9962,6352, 6351,9963, 9964,9965, 9966,9967, 9968 & 
& ,6360, 6359,9969, 9970,9971, 9972], & 
& edgecnc=[2409,604,2410,2411,2412,608,2413,2414], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(327),elname="xbrick",eltype="xbrick",typekey=327) 

        call prepare(lib_xbrick(328),key=328, & 
& nodecnc=[1358,1343,1642,99,3930,3915,4214,2671,9973, 9974,9975, 9976,9977, 9978,9979, 9980,9981, 9982 & 
& ,9983, 9984,9985, 9986,9987, 9988], & 
& edgecnc=[2415,2416,2417,2418,2419,2420,2421,2422], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(328),elname="xbrick",eltype="xbrick",typekey=328) 

        call prepare(lib_xbrick(329),key=329, & 
& nodecnc=[2487,100,1630,1643,5059,2672,4202,4215,9989, 9990,9991, 9992,9993, 9994,8916, 8915,9995, 9996 & 
& ,9997, 9998,9999, 10000,8924, 8923], & 
& edgecnc=[2423,2424,2425,1886,2426,2427,2428,1890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(329),elname="xbrick",eltype="xbrick",typekey=329) 

        call prepare(lib_xbrick(330),key=330, & 
& nodecnc=[1931,1915,2024,2043,4503,4487,4596,4615,10001, 10002,10003, 10004,10005, 10006,10007, 10008 & 
& ,10009, 10010,10011, 10012,10013, 10014,10015, 10016], & 
& edgecnc=[2429,2430,2431,2432,2433,2434,2435,2436], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(330),elname="xbrick",eltype="xbrick",typekey=330) 

        call prepare(lib_xbrick(331),key=331, & 
& nodecnc=[1384,1373,1405,1396,3956,3945,3977,3968,9486, 9485,10017, 10018,10019, 10020,10021, 10022,9494 & 
& , 9493,10023, 10024,10025, 10026,10027, 10028], & 
& edgecnc=[2171,2437,2438,2439,2175,2440,2441,2442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(331),elname="xbrick",eltype="xbrick",typekey=331) 

        call prepare(lib_xbrick(332),key=332, & 
& nodecnc=[148,1396,102,1422,2720,3968,2674,3994,10029, 10030,10031, 10032,10033, 10034,9472, 9471,10035 & 
& , 10036,10037, 10038,10039, 10040,9480, 9479], & 
& edgecnc=[2443,2444,2445,2164,2446,2447,2448,2168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(332),elname="xbrick",eltype="xbrick",typekey=332) 

        call prepare(lib_xbrick(333),key=333, & 
& nodecnc=[1426,1439,1442,149,3998,4011,4014,2721,10041, 10042,9328, 9327,10043, 10044,10045, 10046,10047 & 
& , 10048,9336, 9335,10049, 10050,10051, 10052], & 
& edgecnc=[2449,2092,2450,2451,2452,2096,2453,2454], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(333),elname="xbrick",eltype="xbrick",typekey=333) 

        call prepare(lib_xbrick(334),key=334, & 
& nodecnc=[1833,1710,103,1785,4405,4282,2675,4357,9312, 9311,10053, 10054,10055, 10056,10057, 10058,9318 & 
& , 9317,10059, 10060,10061, 10062,10063, 10064], & 
& edgecnc=[2084,2455,2456,2457,2087,2458,2459,2460], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(334),elname="xbrick",eltype="xbrick",typekey=334) 

        call prepare(lib_xbrick(335),key=335, & 
& nodecnc=[2459,102,1915,1816,5031,2674,4487,4388,10065, 10066,10067, 10068,10069, 10070,10071, 10072 & 
& ,10073, 10074,10075, 10076,10077, 10078,10079, 10080], & 
& edgecnc=[2461,2462,2463,2464,2465,2466,2467,2468], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(335),elname="xbrick",eltype="xbrick",typekey=335) 

        call prepare(lib_xbrick(336),key=336, & 
& nodecnc=[2024,2554,2117,2043,4596,5126,4689,4615,10081, 10082,10083, 10084,9296, 9295,10006, 10005,10085 & 
& , 10086,10087, 10088,9304, 9303,10014, 10013], & 
& edgecnc=[2469,2470,2076,2431,2471,2472,2080,2435], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(336),elname="xbrick",eltype="xbrick",typekey=336) 

        call prepare(lib_xbrick(337),key=337, & 
& nodecnc=[103,1710,1698,1627,2675,4282,4270,4199,10054, 10053,10089, 10090,10091, 10092,10093, 10094 & 
& ,10060, 10059,10095, 10096,10097, 10098,10099, 10100], & 
& edgecnc=[2455,2473,2474,2475,2458,2476,2477,2478], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(337),elname="xbrick",eltype="xbrick",typekey=337) 

        call prepare(lib_xbrick(338),key=338, & 
& nodecnc=[1627,1698,1660,104,4199,4270,4232,2676,10092, 10091,10101, 10102,9212, 9211,10103, 10104,10098 & 
& , 10097,10105, 10106,9218, 9217,10107, 10108], & 
& edgecnc=[2474,2479,2034,2480,2477,2481,2037,2482], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(338),elname="xbrick",eltype="xbrick",typekey=338) 

        call prepare(lib_xbrick(339),key=339, & 
& nodecnc=[1832,1699,105,1770,4404,4271,2677,4342,8996, 8995,10109, 10110,10111, 10112,10113, 10114,9004 & 
& , 9003,10115, 10116,10117, 10118,10119, 10120], & 
& edgecnc=[1926,2483,2484,2485,1930,2486,2487,2488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(339),elname="xbrick",eltype="xbrick",typekey=339) 

        call prepare(lib_xbrick(340),key=340, & 
& nodecnc=[1450,2428,150,1446,4022,5000,2722,4018,9342, 9341,10121, 10122,10123, 10124,10125, 10126,9348 & 
& , 9347,10127, 10128,10129, 10130,10131, 10132], & 
& edgecnc=[2099,2489,2490,2491,2102,2492,2493,2494], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(340),elname="xbrick",eltype="xbrick",typekey=340) 

        call prepare(lib_xbrick(341),key=341, & 
& nodecnc=[1450,1446,1637,1452,4022,4018,4209,4024,10126, 10125,10133, 10134,9072, 9071,9198, 9197,10132 & 
& , 10131,10135, 10136,9080, 9079,9204, 9203], & 
& edgecnc=[2491,2495,1964,2027,2494,2496,1968,2030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(341),elname="xbrick",eltype="xbrick",typekey=341) 

        call prepare(lib_xbrick(342),key=342, & 
& nodecnc=[1873,1874,1859,1832,4445,4446,4431,4404,10137, 10138,10139, 10140,8998, 8997,10141, 10142,10143 & 
& , 10144,10145, 10146,9006, 9005,10147, 10148], & 
& edgecnc=[2497,2498,1927,2499,2500,2501,1931,2502], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(342),elname="xbrick",eltype="xbrick",typekey=342) 

        call prepare(lib_xbrick(343),key=343, & 
& nodecnc=[1991,2203,2195,2068,4563,4775,4767,4640,10149, 10150,10151, 10152,10153, 10154,10155, 10156 & 
& ,10157, 10158,10159, 10160,10161, 10162,10163, 10164], & 
& edgecnc=[2503,2504,2505,2506,2507,2508,2509,2510], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(343),elname="xbrick",eltype="xbrick",typekey=343) 

        call prepare(lib_xbrick(344),key=344, & 
& nodecnc=[2017,154,2086,2034,4589,2726,4658,4606,10165, 10166,10167, 10168,10169, 10170,5846, 5845,10171 & 
& , 10172,10173, 10174,10175, 10176,5854, 5853], & 
& edgecnc=[2511,2512,2513,351,2514,2515,2516,355], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(344),elname="xbrick",eltype="xbrick",typekey=344) 

        call prepare(lib_xbrick(345),key=345, & 
& nodecnc=[92,1681,1739,1979,2664,4253,4311,4551,10177, 10178,10179, 10180,10181, 10182,10183, 10184,10185 & 
& , 10186,10187, 10188,10189, 10190,10191, 10192], & 
& edgecnc=[2517,2518,2519,2520,2521,2522,2523,2524], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(345),elname="xbrick",eltype="xbrick",typekey=345) 

        call prepare(lib_xbrick(346),key=346, & 
& nodecnc=[109,2243,1926,1869,2681,4815,4498,4441,10193, 10194,10195, 10196,9594, 9593,9650, 9649,10197 & 
& , 10198,10199, 10200,9602, 9601,9658, 9657], & 
& edgecnc=[2525,2526,2225,2253,2527,2528,2229,2257], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(346),elname="xbrick",eltype="xbrick",typekey=346) 

        call prepare(lib_xbrick(347),key=347, & 
& nodecnc=[2097,110,2007,2051,4669,2682,4579,4623,9606, 9605,9590, 9589,5936, 5935,10201, 10202,9614, 9613 & 
& ,9598, 9597,5944, 5943,10203, 10204], & 
& edgecnc=[2231,2223,396,2529,2235,2227,400,2530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(347),elname="xbrick",eltype="xbrick",typekey=347) 

        call prepare(lib_xbrick(348),key=348, & 
& nodecnc=[2056,113,2042,2085,4628,2685,4614,4657,10205, 10206,10207, 10208,5798, 5797,10209, 10210,10211 & 
& , 10212,10213, 10214,5806, 5805,10215, 10216], & 
& edgecnc=[2531,2532,327,2533,2534,2535,331,2536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(348),elname="xbrick",eltype="xbrick",typekey=348) 

        call prepare(lib_xbrick(349),key=349, & 
& nodecnc=[1995,114,2499,1988,4567,2686,5071,4560,10217, 10218,10219, 10220,10221, 10222,10223, 10224 & 
& ,10225, 10226,10227, 10228,10229, 10230,10231, 10232], & 
& edgecnc=[2537,2538,2539,2540,2541,2542,2543,2544], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(349),elname="xbrick",eltype="xbrick",typekey=349) 

        call prepare(lib_xbrick(350),key=350, & 
& nodecnc=[1919,2494,198,1982,4491,5066,2770,4554,10233, 10234,10235, 10236,10237, 10238,10239, 10240 & 
& ,10241, 10242,10243, 10244,10245, 10246,10247, 10248], & 
& edgecnc=[2545,2546,2547,2548,2549,2550,2551,2552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(350),elname="xbrick",eltype="xbrick",typekey=350) 

        call prepare(lib_xbrick(351),key=351, & 
& nodecnc=[2386,284,1941,2023,4958,2856,4513,4595,10249, 10250,10251, 10252,10253, 10254,10255, 10256 & 
& ,10257, 10258,10259, 10260,10261, 10262,10263, 10264], & 
& edgecnc=[2553,2554,2555,2556,2557,2558,2559,2560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(351),elname="xbrick",eltype="xbrick",typekey=351) 

        call prepare(lib_xbrick(352),key=352, & 
& nodecnc=[1852,250,1726,1780,4424,2822,4298,4352,10265, 10266,10267, 10268,5768, 5767,10269, 10270,10271 & 
& , 10272,10273, 10274,5776, 5775,10275, 10276], & 
& edgecnc=[2561,2562,312,2563,2564,2565,316,2566], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(352),elname="xbrick",eltype="xbrick",typekey=352) 

        call prepare(lib_xbrick(353),key=353, & 
& nodecnc=[1852,1790,1692,250,4424,4362,4264,2822,10277, 10278,10279, 10280,5756, 5755,10266, 10265,10281 & 
& , 10282,10283, 10284,5764, 5763,10272, 10271], & 
& edgecnc=[2567,2568,306,2561,2569,2570,310,2564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(353),elname="xbrick",eltype="xbrick",typekey=353) 

        call prepare(lib_xbrick(354),key=354, & 
& nodecnc=[1856,249,1692,1790,4428,2821,4264,4362,10285, 10286,10287, 10288,10280, 10279,10289, 10290 & 
& ,10291, 10292,10293, 10294,10284, 10283,10295, 10296], & 
& edgecnc=[2571,2572,2568,2573,2574,2575,2570,2576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(354),elname="xbrick",eltype="xbrick",typekey=354) 

        call prepare(lib_xbrick(355),key=355, & 
& nodecnc=[2427,2073,2067,2154,4999,4645,4639,4726,10297, 10298,9240, 9239,10299, 10300,10301, 10302,10303 & 
& , 10304,9248, 9247,10305, 10306,10307, 10308], & 
& edgecnc=[2577,2048,2578,2579,2580,2052,2581,2582], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(355),elname="xbrick",eltype="xbrick",typekey=355) 

        call prepare(lib_xbrick(356),key=356, & 
& nodecnc=[1941,2076,2055,2023,4513,4648,4627,4595,10309, 10310,10311, 10312,10313, 10314,10254, 10253 & 
& ,10315, 10316,10317, 10318,10319, 10320,10262, 10261], & 
& edgecnc=[2583,2584,2585,2555,2586,2587,2588,2559], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(356),elname="xbrick",eltype="xbrick",typekey=356) 

        call prepare(lib_xbrick(357),key=357, & 
& nodecnc=[1864,116,2534,1919,4436,2688,5106,4491,9226, 9225,10321, 10322,10323, 10324,10325, 10326,9234 & 
& , 9233,10327, 10328,10329, 10330,10331, 10332], & 
& edgecnc=[2041,2589,2590,2591,2045,2592,2593,2594], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(357),elname="xbrick",eltype="xbrick",typekey=357) 

        call prepare(lib_xbrick(358),key=358, & 
& nodecnc=[2535,1930,1368,118,5107,4502,3940,2690,10333, 10334,10335, 10336,10337, 10338,10339, 10340 & 
& ,10341, 10342,10343, 10344,10345, 10346,10347, 10348], & 
& edgecnc=[2595,2596,2597,2598,2599,2600,2601,2602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(358),elname="xbrick",eltype="xbrick",typekey=358) 

        call prepare(lib_xbrick(359),key=359, & 
& nodecnc=[1255,1245,1664,1882,3827,3817,4236,4454,10349, 10350,10351, 10352,10353, 10354,10355, 10356 & 
& ,10357, 10358,10359, 10360,10361, 10362,10363, 10364], & 
& edgecnc=[2603,2604,2605,2606,2607,2608,2609,2610], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(359),elname="xbrick",eltype="xbrick",typekey=359) 

        call prepare(lib_xbrick(360),key=360, & 
& nodecnc=[1003,1045,1019,965,3575,3617,3591,3537,10365, 10366,10367, 10368,9696, 9695,10369, 10370,10371 & 
& , 10372,10373, 10374,9704, 9703,10375, 10376], & 
& edgecnc=[2611,2612,2276,2613,2614,2615,2280,2616], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(360),elname="xbrick",eltype="xbrick",typekey=360) 

        call prepare(lib_xbrick(361),key=361, & 
& nodecnc=[2423,988,2473,206,4995,3560,5045,2778,6408, 6407,10377, 10378,10379, 10380,10381, 10382,6416 & 
& , 6415,10383, 10384,10385, 10386,10387, 10388], & 
& edgecnc=[632,2617,2618,2619,636,2620,2621,2622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(361),elname="xbrick",eltype="xbrick",typekey=361) 

        call prepare(lib_xbrick(362),key=362, & 
& nodecnc=[1086,1093,1122,1206,3658,3665,3694,3778,10389, 10390,10391, 10392,10393, 10394,10395, 10396 & 
& ,10397, 10398,10399, 10400,10401, 10402,10403, 10404], & 
& edgecnc=[2623,2624,2625,2626,2627,2628,2629,2630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(362),elname="xbrick",eltype="xbrick",typekey=362) 

        call prepare(lib_xbrick(363),key=363, & 
& nodecnc=[1663,1616,1665,1732,4235,4188,4237,4304,10405, 10406,10407, 10408,10409, 10410,10411, 10412 & 
& ,10413, 10414,10415, 10416,10417, 10418,10419, 10420], & 
& edgecnc=[2631,2632,2633,2634,2635,2636,2637,2638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(363),elname="xbrick",eltype="xbrick",typekey=363) 

        call prepare(lib_xbrick(364),key=364, & 
& nodecnc=[243,1219,1198,1169,2815,3791,3770,3741,10421, 10422,10423, 10424,10425, 10426,10427, 10428 & 
& ,10429, 10430,10431, 10432,10433, 10434,10435, 10436], & 
& edgecnc=[2639,2640,2641,2642,2643,2644,2645,2646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(364),elname="xbrick",eltype="xbrick",typekey=364) 

        call prepare(lib_xbrick(365),key=365, & 
& nodecnc=[394,393,1544,1528,2966,2965,4116,4100,10437, 10438,10439, 10440,10441, 10442,6364, 6363,10443 & 
& , 10444,10445, 10446,10447, 10448,6372, 6371], & 
& edgecnc=[2647,2648,2649,610,2650,2651,2652,614], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(365),elname="xbrick",eltype="xbrick",typekey=365) 

        call prepare(lib_xbrick(366),key=366, & 
& nodecnc=[1828,1765,1687,124,4400,4337,4259,2696,10449, 10450,7510, 7509,7798, 7797,10451, 10452,10453 & 
& , 10454,7518, 7517,7804, 7803,10455, 10456], & 
& edgecnc=[2653,1183,1327,2654,2655,1187,1330,2656], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(366),elname="xbrick",eltype="xbrick",typekey=366) 

        call prepare(lib_xbrick(367),key=367, & 
& nodecnc=[172,2096,2089,1948,2744,4668,4661,4520,10457, 10458,10459, 10460,10461, 10462,7496, 7495,10463 & 
& , 10464,10465, 10466,10467, 10468,7504, 7503], & 
& edgecnc=[2657,2658,2659,1176,2660,2661,2662,1180], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(367),elname="xbrick",eltype="xbrick",typekey=367) 

        call prepare(lib_xbrick(368),key=368, & 
& nodecnc=[2096,171,2146,2089,4668,2743,4718,4661,10469, 10470,7056, 7055,6930, 6929,10460, 10459,10471 & 
& , 10472,7064, 7063,6938, 6937,10466, 10465], & 
& edgecnc=[2663,956,893,2658,2664,960,897,2661], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(368),elname="xbrick",eltype="xbrick",typekey=368) 

        call prepare(lib_xbrick(369),key=369, & 
& nodecnc=[1947,2361,2088,2094,4519,4933,4660,4666,10473, 10474,10475, 10476,10477, 10478,10479, 10480 & 
& ,10481, 10482,10483, 10484,10485, 10486,10487, 10488], & 
& edgecnc=[2665,2666,2667,2668,2669,2670,2671,2672], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(369),elname="xbrick",eltype="xbrick",typekey=369) 

        call prepare(lib_xbrick(370),key=370, & 
& nodecnc=[2331,2036,2015,1964,4903,4608,4587,4536,10489, 10490,7038, 7037,10491, 10492,10493, 10494,10495 & 
& , 10496,7044, 7043,10497, 10498,10499, 10500], & 
& edgecnc=[2673,947,2674,2675,2676,950,2677,2678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(370),elname="xbrick",eltype="xbrick",typekey=370) 

        call prepare(lib_xbrick(371),key=371, & 
& nodecnc=[126,2180,2168,2393,2698,4752,4740,4965,10501, 10502,10503, 10504,10505, 10506,10507, 10508 & 
& ,10509, 10510,10511, 10512,10513, 10514,10515, 10516], & 
& edgecnc=[2679,2680,2681,2682,2683,2684,2685,2686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(371),elname="xbrick",eltype="xbrick",typekey=371) 

        call prepare(lib_xbrick(372),key=372, & 
& nodecnc=[329,1975,1743,2065,2901,4547,4315,4637,10517, 10518,10519, 10520,10521, 10522,8054, 8053,10523 & 
& , 10524,10525, 10526,10527, 10528,8062, 8061], & 
& edgecnc=[2687,2688,2689,1455,2690,2691,2692,1459], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(372),elname="xbrick",eltype="xbrick",typekey=372) 

        call prepare(lib_xbrick(373),key=373, & 
& nodecnc=[1993,1822,1743,2355,4565,4394,4315,4927,10529, 10530,10531, 10532,10533, 10534,10535, 10536 & 
& ,10537, 10538,10539, 10540,10541, 10542,10543, 10544], & 
& edgecnc=[2693,2694,2695,2696,2697,2698,2699,2700], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(373),elname="xbrick",eltype="xbrick",typekey=373) 

        call prepare(lib_xbrick(374),key=374, & 
& nodecnc=[2080,2029,2081,128,4652,4601,4653,2700,10545, 10546,10547, 10548,10549, 10550,10551, 10552 & 
& ,10553, 10554,10555, 10556,10557, 10558,10559, 10560], & 
& edgecnc=[2701,2702,2703,2704,2705,2706,2707,2708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(374),elname="xbrick",eltype="xbrick",typekey=374) 

        call prepare(lib_xbrick(375),key=375, & 
& nodecnc=[2081,2149,2169,128,4653,4721,4741,2700,10561, 10562,10563, 10564,10565, 10566,10550, 10549 & 
& ,10567, 10568,10569, 10570,10571, 10572,10558, 10557], & 
& edgecnc=[2709,2710,2711,2703,2712,2713,2714,2707], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(375),elname="xbrick",eltype="xbrick",typekey=375) 

        call prepare(lib_xbrick(376),key=376, & 
& nodecnc=[2178,185,2157,2162,4750,2757,4729,4734,10573, 10574,10575, 10576,10577, 10578,10579, 10580 & 
& ,10581, 10582,10583, 10584,10585, 10586,10587, 10588], & 
& edgecnc=[2715,2716,2717,2718,2719,2720,2721,2722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(376),elname="xbrick",eltype="xbrick",typekey=376) 

        call prepare(lib_xbrick(377),key=377, & 
& nodecnc=[184,2069,2130,1965,2756,4641,4702,4537,10589, 10590,10591, 10592,10593, 10594,6778, 6777,10595 & 
& , 10596,10597, 10598,10599, 10600,6786, 6785], & 
& edgecnc=[2723,2724,2725,817,2726,2727,2728,821], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(377),elname="xbrick",eltype="xbrick",typekey=377) 

        call prepare(lib_xbrick(378),key=378, & 
& nodecnc=[219,2031,263,2116,2791,4603,2835,4688,10601, 10602,10603, 10604,10605, 10606,10607, 10608,10609 & 
& , 10610,10611, 10612,10613, 10614,10615, 10616], & 
& edgecnc=[2729,2730,2731,2732,2733,2734,2735,2736], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(378),elname="xbrick",eltype="xbrick",typekey=378) 

        call prepare(lib_xbrick(379),key=379, & 
& nodecnc=[2422,2030,2044,2421,4994,4602,4616,4993,10617, 10618,10619, 10620,10621, 10622,10623, 10624 & 
& ,10625, 10626,10627, 10628,10629, 10630,10631, 10632], & 
& edgecnc=[2737,2738,2739,2740,2741,2742,2743,2744], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(379),elname="xbrick",eltype="xbrick",typekey=379) 

        call prepare(lib_xbrick(380),key=380, & 
& nodecnc=[1822,1759,1773,330,4394,4331,4345,2902,10633, 10634,10635, 10636,10637, 10638,10639, 10640 & 
& ,10641, 10642,10643, 10644,10645, 10646,10647, 10648], & 
& edgecnc=[2745,2746,2747,2748,2749,2750,2751,2752], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(380),elname="xbrick",eltype="xbrick",typekey=380) 

        call prepare(lib_xbrick(381),key=381, & 
& nodecnc=[1837,331,1773,1759,4409,2903,4345,4331,8028, 8027,10649, 10650,10636, 10635,10651, 10652,8036 & 
& , 8035,10653, 10654,10644, 10643,10655, 10656], & 
& edgecnc=[1442,2753,2746,2754,1446,2755,2750,2756], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(381),elname="xbrick",eltype="xbrick",typekey=381) 

        call prepare(lib_xbrick(382),key=382, & 
& nodecnc=[2077,2028,2078,2490,4649,4600,4650,5062,10657, 10658,10659, 10660,10661, 10662,10663, 10664 & 
& ,10665, 10666,10667, 10668,10669, 10670,10671, 10672], & 
& edgecnc=[2757,2758,2759,2760,2761,2762,2763,2764], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(382),elname="xbrick",eltype="xbrick",typekey=382) 

        call prepare(lib_xbrick(383),key=383, & 
& nodecnc=[2114,2157,185,2070,4686,4729,2757,4642,10673, 10674,10576, 10575,10675, 10676,10677, 10678 & 
& ,10679, 10680,10584, 10583,10681, 10682,10683, 10684], & 
& edgecnc=[2765,2716,2766,2767,2768,2720,2769,2770], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(383),elname="xbrick",eltype="xbrick",typekey=383) 

        call prepare(lib_xbrick(384),key=384, & 
& nodecnc=[2106,2148,2105,2533,4678,4720,4677,5105,10685, 10686,10687, 10688,6760, 6759,10689, 10690,10691 & 
& , 10692,10693, 10694,6768, 6767,10695, 10696], & 
& edgecnc=[2771,2772,808,2773,2774,2775,812,2776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(384),elname="xbrick",eltype="xbrick",typekey=384) 

        call prepare(lib_xbrick(385),key=385, & 
& nodecnc=[2110,2026,2436,1956,4682,4598,5008,4528,10697, 10698,10699, 10700,10701, 10702,10703, 10704 & 
& ,10705, 10706,10707, 10708,10709, 10710,10711, 10712], & 
& edgecnc=[2777,2778,2779,2780,2781,2782,2783,2784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(385),elname="xbrick",eltype="xbrick",typekey=385) 

        call prepare(lib_xbrick(386),key=386, & 
& nodecnc=[1851,1789,1693,332,4423,4361,4265,2904,10713, 10714,10715, 10716,10717, 10718,10719, 10720 & 
& ,10721, 10722,10723, 10724,10725, 10726,10727, 10728], & 
& edgecnc=[2785,2786,2787,2788,2789,2790,2791,2792], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(386),elname="xbrick",eltype="xbrick",typekey=386) 

        call prepare(lib_xbrick(387),key=387, & 
& nodecnc=[1693,1573,1572,332,4265,4145,4144,2904,8122, 8121,10729, 10730,8102, 8101,10718, 10717,8130 & 
& , 8129,10731, 10732,8110, 8109,10726, 10725], & 
& edgecnc=[1489,2793,1479,2787,1493,2794,1483,2791], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(387),elname="xbrick",eltype="xbrick",typekey=387) 

        call prepare(lib_xbrick(388),key=388, & 
& nodecnc=[2141,129,2022,2489,4713,2701,4594,5061,10733, 10734,10735, 10736,10737, 10738,10739, 10740 & 
& ,10741, 10742,10743, 10744,10745, 10746,10747, 10748], & 
& edgecnc=[2795,2796,2797,2798,2799,2800,2801,2802], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(388),elname="xbrick",eltype="xbrick",typekey=388) 

        call prepare(lib_xbrick(389),key=389, & 
& nodecnc=[2148,2106,2158,129,4720,4678,4730,2701,10686, 10685,10749, 10750,10751, 10752,10753, 10754 & 
& ,10692, 10691,10755, 10756,10757, 10758,10759, 10760], & 
& edgecnc=[2771,2803,2804,2805,2774,2806,2807,2808], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(389),elname="xbrick",eltype="xbrick",typekey=389) 

        call prepare(lib_xbrick(390),key=390, & 
& nodecnc=[2465,1960,2020,1940,5037,4532,4592,4512,10761, 10762,10763, 10764,10765, 10766,10767, 10768 & 
& ,10769, 10770,10771, 10772,10773, 10774,10775, 10776], & 
& edgecnc=[2809,2810,2811,2812,2813,2814,2815,2816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(390),elname="xbrick",eltype="xbrick",typekey=390) 

        call prepare(lib_xbrick(391),key=391, & 
& nodecnc=[131,2020,2026,1918,2703,4592,4598,4490,10777, 10778,10779, 10780,10781, 10782,10783, 10784 & 
& ,10785, 10786,10787, 10788,10789, 10790,10791, 10792], & 
& edgecnc=[2817,2818,2819,2820,2821,2822,2823,2824], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(391),elname="xbrick",eltype="xbrick",typekey=391) 

        call prepare(lib_xbrick(392),key=392, & 
& nodecnc=[1339,1314,2500,1301,3911,3886,5072,3873,10793, 10794,10795, 10796,10797, 10798,10799, 10800 & 
& ,10801, 10802,10803, 10804,10805, 10806,10807, 10808], & 
& edgecnc=[2825,2826,2827,2828,2829,2830,2831,2832], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(392),elname="xbrick",eltype="xbrick",typekey=392) 

        call prepare(lib_xbrick(393),key=393, & 
& nodecnc=[1379,1365,163,1391,3951,3937,2735,3963,10809, 10810,10811, 10812,10813, 10814,10815, 10816 & 
& ,10817, 10818,10819, 10820,10821, 10822,10823, 10824], & 
& edgecnc=[2833,2834,2835,2836,2837,2838,2839,2840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(393),elname="xbrick",eltype="xbrick",typekey=393) 

        call prepare(lib_xbrick(394),key=394, & 
& nodecnc=[1418,2529,182,1412,3990,5101,2754,3984,10825, 10826,10827, 10828,10829, 10830,10831, 10832 & 
& ,10833, 10834,10835, 10836,10837, 10838,10839, 10840], & 
& edgecnc=[2841,2842,2843,2844,2845,2846,2847,2848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(394),elname="xbrick",eltype="xbrick",typekey=394) 

        call prepare(lib_xbrick(395),key=395, & 
& nodecnc=[1628,1648,1417,2505,4200,4220,3989,5077,10841, 10842,10843, 10844,10845, 10846,10847, 10848 & 
& ,10849, 10850,10851, 10852,10853, 10854,10855, 10856], & 
& edgecnc=[2849,2850,2851,2852,2853,2854,2855,2856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(395),elname="xbrick",eltype="xbrick",typekey=395) 

        call prepare(lib_xbrick(396),key=396, & 
& nodecnc=[1709,132,1858,1804,4281,2704,4430,4376,10857, 10858,10859, 10860,6742, 6741,7190, 7189,10861 & 
& , 10862,10863, 10864,6750, 6749,7198, 7197], & 
& edgecnc=[2857,2858,799,1023,2859,2860,803,1027], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(396),elname="xbrick",eltype="xbrick",typekey=396) 

        call prepare(lib_xbrick(397),key=397, & 
& nodecnc=[134,1254,1290,1274,2706,3826,3862,3846,10865, 10866,10867, 10868,10869, 10870,10871, 10872 & 
& ,10873, 10874,10875, 10876,10877, 10878,10879, 10880], & 
& edgecnc=[2861,2862,2863,2864,2865,2866,2867,2868], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(397),elname="xbrick",eltype="xbrick",typekey=397) 

        call prepare(lib_xbrick(398),key=398, & 
& nodecnc=[1315,1884,132,1301,3887,4456,2704,3873,10881, 10882,10883, 10884,10885, 10886,10887, 10888 & 
& ,10889, 10890,10891, 10892,10893, 10894,10895, 10896], & 
& edgecnc=[2869,2870,2871,2872,2873,2874,2875,2876], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(398),elname="xbrick",eltype="xbrick",typekey=398) 

        call prepare(lib_xbrick(399),key=399, & 
& nodecnc=[1268,1420,1423,338,3840,3992,3995,2910,8550, 8549,10897, 10898,10899, 10900,10901, 10902,8558 & 
& , 8557,10903, 10904,10905, 10906,10907, 10908], & 
& edgecnc=[1703,2877,2878,2879,1707,2880,2881,2882], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(399),elname="xbrick",eltype="xbrick",typekey=399) 

        call prepare(lib_xbrick(400),key=400, & 
& nodecnc=[1747,1701,1641,1632,4319,4273,4213,4204,7158, 7157,10909, 10910,10911, 10912,10913, 10914,7166 & 
& , 7165,10915, 10916,10917, 10918,10919, 10920], & 
& edgecnc=[1007,2883,2884,2885,1011,2886,2887,2888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(400),elname="xbrick",eltype="xbrick",typekey=400) 

        call prepare(lib_xbrick(401),key=401, & 
& nodecnc=[134,1274,133,1265,2706,3846,2705,3837,10872, 10871,10921, 10922,10923, 10924,10925, 10926,10880 & 
& , 10879,10927, 10928,10929, 10930,10931, 10932], & 
& edgecnc=[2864,2889,2890,2891,2868,2892,2893,2894], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(401),elname="xbrick",eltype="xbrick",typekey=401) 

        call prepare(lib_xbrick(402),key=402, & 
& nodecnc=[1300,1271,1265,133,3872,3843,3837,2705,10933, 10934,10935, 10936,10924, 10923,10937, 10938 & 
& ,10939, 10940,10941, 10942,10930, 10929,10943, 10944], & 
& edgecnc=[2895,2896,2890,2897,2898,2899,2893,2900], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(402),elname="xbrick",eltype="xbrick",typekey=402) 

        call prepare(lib_xbrick(403),key=403, & 
& nodecnc=[1295,1624,1657,1283,3867,4196,4229,3855,10945, 10946,10947, 10948,10949, 10950,10951, 10952 & 
& ,10953, 10954,10955, 10956,10957, 10958,10959, 10960], & 
& edgecnc=[2901,2902,2903,2904,2905,2906,2907,2908], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(403),elname="xbrick",eltype="xbrick",typekey=403) 

        call prepare(lib_xbrick(404),key=404, & 
& nodecnc=[1297,1269,1263,1275,3869,3841,3835,3847,10961, 10962,10963, 10964,10965, 10966,10967, 10968 & 
& ,10969, 10970,10971, 10972,10973, 10974,10975, 10976], & 
& edgecnc=[2909,2910,2911,2912,2913,2914,2915,2916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(404),elname="xbrick",eltype="xbrick",typekey=404) 

        call prepare(lib_xbrick(405),key=405, & 
& nodecnc=[2322,1317,2304,1305,4894,3889,4876,3877,10977, 10978,10979, 10980,10981, 10982,10983, 10984 & 
& ,10985, 10986,10987, 10988,10989, 10990,10991, 10992], & 
& edgecnc=[2917,2918,2919,2920,2921,2922,2923,2924], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(405),elname="xbrick",eltype="xbrick",typekey=405) 

        call prepare(lib_xbrick(406),key=406, & 
& nodecnc=[1623,2255,235,1388,4195,4827,2807,3960,10993, 10994,10995, 10996,10997, 10998,10999, 11000 & 
& ,11001, 11002,11003, 11004,11005, 11006,11007, 11008], & 
& edgecnc=[2925,2926,2927,2928,2929,2930,2931,2932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(406),elname="xbrick",eltype="xbrick",typekey=406) 

        call prepare(lib_xbrick(407),key=407, & 
& nodecnc=[40,404,1617,1651,2612,2976,4189,4223,11009, 11010,11011, 11012,11013, 11014,11015, 11016,11017 & 
& , 11018,11019, 11020,11021, 11022,11023, 11024], & 
& edgecnc=[2933,2934,2935,2936,2937,2938,2939,2940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(407),elname="xbrick",eltype="xbrick",typekey=407) 

        call prepare(lib_xbrick(408),key=408, & 
& nodecnc=[1366,1408,2211,1370,3938,3980,4783,3942,11025, 11026,11027, 11028,11029, 11030,11031, 11032 & 
& ,11033, 11034,11035, 11036,11037, 11038,11039, 11040], & 
& edgecnc=[2941,2942,2943,2944,2945,2946,2947,2948], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(408),elname="xbrick",eltype="xbrick",typekey=408) 

        call prepare(lib_xbrick(409),key=409, & 
& nodecnc=[1332,1356,2231,1344,3904,3928,4803,3916,11041, 11042,11043, 11044,11045, 11046,11047, 11048 & 
& ,11049, 11050,11051, 11052,11053, 11054,11055, 11056], & 
& edgecnc=[2949,2950,2951,2952,2953,2954,2955,2956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(409),elname="xbrick",eltype="xbrick",typekey=409) 

        call prepare(lib_xbrick(410),key=410, & 
& nodecnc=[2362,1131,1123,1306,4934,3703,3695,3878,11057, 11058,11059, 11060,11061, 11062,11063, 11064 & 
& ,11065, 11066,11067, 11068,11069, 11070,11071, 11072], & 
& edgecnc=[2957,2958,2959,2960,2961,2962,2963,2964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(410),elname="xbrick",eltype="xbrick",typekey=410) 

        call prepare(lib_xbrick(411),key=411, & 
& nodecnc=[1243,1263,1269,177,3815,3835,3841,2749,11073, 11074,10964, 10963,11075, 11076,11077, 11078 & 
& ,11079, 11080,10972, 10971,11081, 11082,11083, 11084], & 
& edgecnc=[2965,2910,2966,2967,2968,2914,2969,2970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(411),elname="xbrick",eltype="xbrick",typekey=411) 

        call prepare(lib_xbrick(412),key=412, & 
& nodecnc=[1161,160,1225,1230,3733,2732,3797,3802,11085, 11086,11087, 11088,11089, 11090,11091, 11092 & 
& ,11093, 11094,11095, 11096,11097, 11098,11099, 11100], & 
& edgecnc=[2971,2972,2973,2974,2975,2976,2977,2978], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(412),elname="xbrick",eltype="xbrick",typekey=412) 

        call prepare(lib_xbrick(413),key=413, & 
& nodecnc=[1192,1213,160,1174,3764,3785,2732,3746,11101, 11102,11103, 11104,11105, 11106,11107, 11108 & 
& ,11109, 11110,11111, 11112,11113, 11114,11115, 11116], & 
& edgecnc=[2979,2980,2981,2982,2983,2984,2985,2986], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(413),elname="xbrick",eltype="xbrick",typekey=413) 

        call prepare(lib_xbrick(414),key=414, & 
& nodecnc=[1322,1209,1241,1249,3894,3781,3813,3821,11117, 11118,11119, 11120,11121, 11122,11123, 11124 & 
& ,11125, 11126,11127, 11128,11129, 11130,11131, 11132], & 
& edgecnc=[2987,2988,2989,2990,2991,2992,2993,2994], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(414),elname="xbrick",eltype="xbrick",typekey=414) 

        call prepare(lib_xbrick(415),key=415, & 
& nodecnc=[1420,337,1430,1423,3992,2909,4002,3995,11133, 11134,11135, 11136,11137, 11138,10898, 10897 & 
& ,11139, 11140,11141, 11142,11143, 11144,10904, 10903], & 
& edgecnc=[2995,2996,2997,2877,2998,2999,3000,2880], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(415),elname="xbrick",eltype="xbrick",typekey=415) 

        call prepare(lib_xbrick(416),key=416, & 
& nodecnc=[1177,2540,1112,1162,3749,5112,3684,3734,11145, 11146,11147, 11148,11149, 11150,11151, 11152 & 
& ,11153, 11154,11155, 11156,11157, 11158,11159, 11160], & 
& edgecnc=[3001,3002,3003,3004,3005,3006,3007,3008], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(416),elname="xbrick",eltype="xbrick",typekey=416) 

        call prepare(lib_xbrick(417),key=417, & 
& nodecnc=[896,921,966,942,3468,3493,3538,3514,11161, 11162,11163, 11164,8298, 8297,11165, 11166,11167 & 
& , 11168,11169, 11170,8306, 8305,11171, 11172], & 
& edgecnc=[3009,3010,1577,3011,3012,3013,1581,3014], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(417),elname="xbrick",eltype="xbrick",typekey=417) 

        call prepare(lib_xbrick(418),key=418, & 
& nodecnc=[1124,1043,1006,1095,3696,3615,3578,3667,11173, 11174,11175, 11176,11177, 11178,11179, 11180 & 
& ,11181, 11182,11183, 11184,11185, 11186,11187, 11188], & 
& edgecnc=[3015,3016,3017,3018,3019,3020,3021,3022], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(418),elname="xbrick",eltype="xbrick",typekey=418) 

        call prepare(lib_xbrick(419),key=419, & 
& nodecnc=[1167,1150,2413,2414,3739,3722,4985,4986,8520, 8519,11189, 11190,11191, 11192,11193, 11194,8528 & 
& , 8527,11195, 11196,11197, 11198,11199, 11200], & 
& edgecnc=[1688,3023,3024,3025,1692,3026,3027,3028], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(419),elname="xbrick",eltype="xbrick",typekey=419) 

        call prepare(lib_xbrick(420),key=420, & 
& nodecnc=[782,711,684,748,3354,3283,3256,3320,11201, 11202,11203, 11204,11205, 11206,11207, 11208,11209 & 
& , 11210,11211, 11212,11213, 11214,11215, 11216], & 
& edgecnc=[3029,3030,3031,3032,3033,3034,3035,3036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(420),elname="xbrick",eltype="xbrick",typekey=420) 

        call prepare(lib_xbrick(421),key=421, & 
& nodecnc=[751,801,830,140,3323,3373,3402,2712,11217, 11218,11219, 11220,11221, 11222,11223, 11224,11225 & 
& , 11226,11227, 11228,11229, 11230,11231, 11232], & 
& edgecnc=[3037,3038,3039,3040,3041,3042,3043,3044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(421),elname="xbrick",eltype="xbrick",typekey=421) 

        call prepare(lib_xbrick(422),key=422, & 
& nodecnc=[864,900,948,895,3436,3472,3520,3467,11233, 11234,11235, 11236,11237, 11238,11239, 11240,11241 & 
& , 11242,11243, 11244,11245, 11246,11247, 11248], & 
& edgecnc=[3045,3046,3047,3048,3049,3050,3051,3052], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(422),elname="xbrick",eltype="xbrick",typekey=422) 

        call prepare(lib_xbrick(423),key=423, & 
& nodecnc=[1120,1153,1159,1170,3692,3725,3731,3742,11249, 11250,11251, 11252,11253, 11254,8732, 8731,11255 & 
& , 11256,11257, 11258,11259, 11260,8740, 8739], & 
& edgecnc=[3053,3054,3055,1794,3056,3057,3058,1798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(423),elname="xbrick",eltype="xbrick",typekey=423) 

        call prepare(lib_xbrick(424),key=424, & 
& nodecnc=[1313,1299,2233,1287,3885,3871,4805,3859,11261, 11262,11263, 11264,11265, 11266,11267, 11268 & 
& ,11269, 11270,11271, 11272,11273, 11274,11275, 11276], & 
& edgecnc=[3059,3060,3061,3062,3063,3064,3065,3066], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(424),elname="xbrick",eltype="xbrick",typekey=424) 

        call prepare(lib_xbrick(425),key=425, & 
& nodecnc=[1313,1281,464,463,3885,3853,3036,3035,11277, 11278,11279, 11280,11281, 11282,11283, 11284,11285 & 
& , 11286,11287, 11288,11289, 11290,11291, 11292], & 
& edgecnc=[3067,3068,3069,3070,3071,3072,3073,3074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(425),elname="xbrick",eltype="xbrick",typekey=425) 

        call prepare(lib_xbrick(426),key=426, & 
& nodecnc=[1081,1109,1139,1061,3653,3681,3711,3633,11293, 11294,11295, 11296,11297, 11298,11299, 11300 & 
& ,11301, 11302,11303, 11304,11305, 11306,11307, 11308], & 
& edgecnc=[3075,3076,3077,3078,3079,3080,3081,3082], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(426),elname="xbrick",eltype="xbrick",typekey=426) 

        call prepare(lib_xbrick(427),key=427, & 
& nodecnc=[1214,1163,1138,1115,3786,3735,3710,3687,11309, 11310,11311, 11312,11313, 11314,11315, 11316 & 
& ,11317, 11318,11319, 11320,11321, 11322,11323, 11324], & 
& edgecnc=[3083,3084,3085,3086,3087,3088,3089,3090], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(427),elname="xbrick",eltype="xbrick",typekey=427) 

        call prepare(lib_xbrick(428),key=428, & 
& nodecnc=[1108,1001,1084,1078,3680,3573,3656,3650,11325, 11326,11327, 11328,11329, 11330,11331, 11332 & 
& ,11333, 11334,11335, 11336,11337, 11338,11339, 11340], & 
& edgecnc=[3091,3092,3093,3094,3095,3096,3097,3098], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(428),elname="xbrick",eltype="xbrick",typekey=428) 

        call prepare(lib_xbrick(429),key=429, & 
& nodecnc=[1631,1246,144,1231,4203,3818,2716,3803,11341, 11342,9882, 9881,11343, 11344,11345, 11346,11347 & 
& , 11348,9890, 9889,11349, 11350,11351, 11352], & 
& edgecnc=[3099,2369,3100,3101,3102,2373,3103,3104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(429),elname="xbrick",eltype="xbrick",typekey=429) 

        call prepare(lib_xbrick(430),key=430, & 
& nodecnc=[1294,145,1310,1296,3866,2717,3882,3868,11353, 11354,11355, 11356,5662, 5661,11357, 11358,11359 & 
& , 11360,11361, 11362,5668, 5667,11363, 11364], & 
& edgecnc=[3105,3106,259,3107,3108,3109,262,3110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(430),elname="xbrick",eltype="xbrick",typekey=430) 

        call prepare(lib_xbrick(431),key=431, & 
& nodecnc=[2487,1353,1348,2486,5059,3925,3920,5058,8914, 8913,11365, 11366,11367, 11368,11369, 11370,8922 & 
& , 8921,11371, 11372,11373, 11374,11375, 11376], & 
& edgecnc=[1885,3111,3112,3113,1889,3114,3115,3116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(431),elname="xbrick",eltype="xbrick",typekey=431) 

        call prepare(lib_xbrick(432),key=432, & 
& nodecnc=[1426,149,1436,1433,3998,2721,4008,4005,10046, 10045,11377, 11378,11379, 11380,9468, 9467,10052 & 
& , 10051,11381, 11382,11383, 11384,9476, 9475], & 
& edgecnc=[2451,3117,3118,2162,2454,3119,3120,2166], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(432),elname="xbrick",eltype="xbrick",typekey=432) 

        call prepare(lib_xbrick(433),key=433, & 
& nodecnc=[1443,1437,1436,149,4015,4009,4008,2721,11385, 11386,11387, 11388,11378, 11377,11389, 11390 & 
& ,11391, 11392,11393, 11394,11382, 11381,11395, 11396], & 
& edgecnc=[3121,3122,3117,3123,3124,3125,3119,3126], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(433),elname="xbrick",eltype="xbrick",typekey=433) 

        call prepare(lib_xbrick(434),key=434, & 
& nodecnc=[1445,1443,149,1442,4017,4015,2721,4014,11397, 11398,11390, 11389,10044, 10043,11399, 11400 & 
& ,11401, 11402,11396, 11395,10050, 10049,11403, 11404], & 
& edgecnc=[3127,3123,2450,3128,3129,3126,2453,3130], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(434),elname="xbrick",eltype="xbrick",typekey=434) 

        call prepare(lib_xbrick(435),key=435, & 
& nodecnc=[1888,152,1808,1939,4460,2724,4380,4511,9098, 9097,11405, 11406,11407, 11408,11409, 11410,9106 & 
& , 9105,11411, 11412,11413, 11414,11415, 11416], & 
& edgecnc=[1977,3131,3132,3133,1981,3134,3135,3136], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(435),elname="xbrick",eltype="xbrick",typekey=435) 

        call prepare(lib_xbrick(436),key=436, & 
& nodecnc=[2336,1860,2372,1662,4908,4432,4944,4234,11417, 11418,11419, 11420,11421, 11422,11423, 11424 & 
& ,11425, 11426,11427, 11428,11429, 11430,11431, 11432], & 
& edgecnc=[3137,3138,3139,3140,3141,3142,3143,3144], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(436),elname="xbrick",eltype="xbrick",typekey=436) 

        call prepare(lib_xbrick(437),key=437, & 
& nodecnc=[1662,151,1702,2336,4234,2723,4274,4908,11433, 11434,9088, 9087,11435, 11436,11424, 11423,11437 & 
& , 11438,9096, 9095,11439, 11440,11432, 11431], & 
& edgecnc=[3145,1972,3146,3140,3147,1976,3148,3144], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(437),elname="xbrick",eltype="xbrick",typekey=437) 

        call prepare(lib_xbrick(438),key=438, & 
& nodecnc=[1841,1762,1678,324,4413,4334,4250,2896,11441, 11442,11443, 11444,11445, 11446,11447, 11448 & 
& ,11449, 11450,11451, 11452,11453, 11454,11455, 11456], & 
& edgecnc=[3149,3150,3151,3152,3153,3154,3155,3156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(438),elname="xbrick",eltype="xbrick",typekey=438) 

        call prepare(lib_xbrick(439),key=439, & 
& nodecnc=[1826,156,1678,1762,4398,2728,4250,4334,11457, 11458,11459, 11460,11444, 11443,11461, 11462 & 
& ,11463, 11464,11465, 11466,11452, 11451,11467, 11468], & 
& edgecnc=[3157,3158,3150,3159,3160,3161,3154,3162], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(439),elname="xbrick",eltype="xbrick",typekey=439) 

        call prepare(lib_xbrick(440),key=440, & 
& nodecnc=[513,534,571,576,3085,3106,3143,3148,5304, 5303,11469, 11470,11471, 11472,11473, 11474,5312 & 
& , 5311,11475, 11476,11477, 11478,11479, 11480], & 
& edgecnc=[80,3163,3164,3165,84,3166,3167,3168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(440),elname="xbrick",eltype="xbrick",typekey=440) 

        call prepare(lib_xbrick(441),key=441, & 
& nodecnc=[1102,1128,1079,1060,3674,3700,3651,3632,11481, 11482,11483, 11484,8470, 8469,8490, 8489,11485 & 
& , 11486,11487, 11488,8478, 8477,8498, 8497], & 
& edgecnc=[3169,3170,1663,1673,3171,3172,1667,1677], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(441),elname="xbrick",eltype="xbrick",typekey=441) 

        call prepare(lib_xbrick(442),key=442, & 
& nodecnc=[1647,1328,1314,1339,4219,3900,3886,3911,11489, 11490,11491, 11492,10794, 10793,11493, 11494 & 
& ,11495, 11496,11497, 11498,10802, 10801,11499, 11500], & 
& edgecnc=[3173,3174,2825,3175,3176,3177,2829,3178], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(442),elname="xbrick",eltype="xbrick",typekey=442) 

        call prepare(lib_xbrick(443),key=443, & 
& nodecnc=[1320,1329,1345,179,3892,3901,3917,2751,11501, 11502,11503, 11504,11505, 11506,11507, 11508 & 
& ,11509, 11510,11511, 11512,11513, 11514,11515, 11516], & 
& edgecnc=[3179,3180,3181,3182,3183,3184,3185,3186], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(443),elname="xbrick",eltype="xbrick",typekey=443) 

        call prepare(lib_xbrick(444),key=444, & 
& nodecnc=[1365,1379,1390,180,3937,3951,3962,2752,10810, 10809,11517, 11518,11519, 11520,11521, 11522 & 
& ,10818, 10817,11523, 11524,11525, 11526,11527, 11528], & 
& edgecnc=[2833,3187,3188,3189,2837,3190,3191,3192], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(444),elname="xbrick",eltype="xbrick",typekey=444) 

        call prepare(lib_xbrick(445),key=445, & 
& nodecnc=[1380,2392,1393,2333,3952,4964,3965,4905,11529, 11530,11531, 11532,11533, 11534,11535, 11536 & 
& ,11537, 11538,11539, 11540,11541, 11542,11543, 11544], & 
& edgecnc=[3193,3194,3195,3196,3197,3198,3199,3200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(445),elname="xbrick",eltype="xbrick",typekey=445) 

        call prepare(lib_xbrick(446),key=446, & 
& nodecnc=[163,1365,2492,2530,2735,3937,5064,5102,10812, 10811,11545, 11546,11547, 11548,11549, 11550 & 
& ,10820, 10819,11551, 11552,11553, 11554,11555, 11556], & 
& edgecnc=[2834,3201,3202,3203,2838,3204,3205,3206], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(446),elname="xbrick",eltype="xbrick",typekey=446) 

        call prepare(lib_xbrick(447),key=447, & 
& nodecnc=[2446,1320,1311,2445,5018,3892,3883,5017,11557, 11558,11559, 11560,11561, 11562,11563, 11564 & 
& ,11565, 11566,11567, 11568,11569, 11570,11571, 11572], & 
& edgecnc=[3207,3208,3209,3210,3211,3212,3213,3214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(447),elname="xbrick",eltype="xbrick",typekey=447) 

        call prepare(lib_xbrick(448),key=448, & 
& nodecnc=[1321,213,1361,1371,3893,2785,3933,3943,11573, 11574,11575, 11576,11577, 11578,11579, 11580 & 
& ,11581, 11582,11583, 11584,11585, 11586,11587, 11588], & 
& edgecnc=[3215,3216,3217,3218,3219,3220,3221,3222], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(448),elname="xbrick",eltype="xbrick",typekey=448) 

        call prepare(lib_xbrick(449),key=449, & 
& nodecnc=[1409,1406,1397,1400,3981,3978,3969,3972,11589, 11590,11591, 11592,11593, 11594,11595, 11596 & 
& ,11597, 11598,11599, 11600,11601, 11602,11603, 11604], & 
& edgecnc=[3223,3224,3225,3226,3227,3228,3229,3230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(449),elname="xbrick",eltype="xbrick",typekey=449) 

        call prepare(lib_xbrick(450),key=450, & 
& nodecnc=[236,1388,1435,1398,2808,3960,4007,3970,11605, 11606,11607, 11608,11609, 11610,11611, 11612 & 
& ,11613, 11614,11615, 11616,11617, 11618,11619, 11620], & 
& edgecnc=[3231,3232,3233,3234,3235,3236,3237,3238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(450),elname="xbrick",eltype="xbrick",typekey=450) 

        call prepare(lib_xbrick(451),key=451, & 
& nodecnc=[408,407,1416,1476,2980,2979,3988,4048,11621, 11622,11623, 11624,11625, 11626,11627, 11628,11629 & 
& , 11630,11631, 11632,11633, 11634,11635, 11636], & 
& edgecnc=[3239,3240,3241,3242,3243,3244,3245,3246], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(451),elname="xbrick",eltype="xbrick",typekey=451) 

        call prepare(lib_xbrick(452),key=452, & 
& nodecnc=[1476,1416,1414,1717,4048,3988,3986,4289,11626, 11625,11637, 11638,11639, 11640,11641, 11642 & 
& ,11634, 11633,11643, 11644,11645, 11646,11647, 11648], & 
& edgecnc=[3241,3247,3248,3249,3245,3250,3251,3252], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(452),elname="xbrick",eltype="xbrick",typekey=452) 

        call prepare(lib_xbrick(453),key=453, & 
& nodecnc=[1401,1389,2252,1377,3973,3961,4824,3949,11649, 11650,11651, 11652,11653, 11654,11655, 11656 & 
& ,11657, 11658,11659, 11660,11661, 11662,11663, 11664], & 
& edgecnc=[3253,3254,3255,3256,3257,3258,3259,3260], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(453),elname="xbrick",eltype="xbrick",typekey=453) 

        call prepare(lib_xbrick(454),key=454, & 
& nodecnc=[1347,214,1403,1352,3919,2786,3975,3924,11665, 11666,11667, 11668,11669, 11670,11671, 11672 & 
& ,11673, 11674,11675, 11676,11677, 11678,11679, 11680], & 
& edgecnc=[3261,3262,3263,3264,3265,3266,3267,3268], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(454),elname="xbrick",eltype="xbrick",typekey=454) 

        call prepare(lib_xbrick(455),key=455, & 
& nodecnc=[1347,1336,1351,1360,3919,3908,3923,3932,11681, 11682,11683, 11684,11685, 11686,11687, 11688 & 
& ,11689, 11690,11691, 11692,11693, 11694,11695, 11696], & 
& edgecnc=[3269,3270,3271,3272,3273,3274,3275,3276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(455),elname="xbrick",eltype="xbrick",typekey=455) 

        call prepare(lib_xbrick(456),key=456, & 
& nodecnc=[163,2553,1644,1391,2735,5125,4216,3963,11697, 11698,11699, 11700,11701, 11702,10814, 10813 & 
& ,11703, 11704,11705, 11706,11707, 11708,10822, 10821], & 
& edgecnc=[3277,3278,3279,2835,3280,3281,3282,2839], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(456),elname="xbrick",eltype="xbrick",typekey=456) 

        call prepare(lib_xbrick(457),key=457, & 
& nodecnc=[164,1866,1974,1425,2736,4438,4546,3997,11709, 11710,11711, 11712,11713, 11714,11715, 11716 & 
& ,11717, 11718,11719, 11720,11721, 11722,11723, 11724], & 
& edgecnc=[3283,3284,3285,3286,3287,3288,3289,3290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(457),elname="xbrick",eltype="xbrick",typekey=457) 

        call prepare(lib_xbrick(458),key=458, & 
& nodecnc=[2140,2109,2041,166,4712,4681,4613,2738,11725, 11726,11727, 11728,11729, 11730,11731, 11732 & 
& ,11733, 11734,11735, 11736,11737, 11738,11739, 11740], & 
& edgecnc=[3291,3292,3293,3294,3295,3296,3297,3298], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(458),elname="xbrick",eltype="xbrick",typekey=458) 

        call prepare(lib_xbrick(459),key=459, & 
& nodecnc=[184,2013,2114,2070,2756,4585,4686,4642,11741, 11742,11743, 11744,10678, 10677,11745, 11746 & 
& ,11747, 11748,11749, 11750,10684, 10683,11751, 11752], & 
& edgecnc=[3299,3300,2767,3301,3302,3303,2770,3304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(459),elname="xbrick",eltype="xbrick",typekey=459) 

        call prepare(lib_xbrick(460),key=460, & 
& nodecnc=[168,2187,2162,2157,2740,4759,4734,4729,11753, 11754,11755, 11756,10578, 10577,11757, 11758 & 
& ,11759, 11760,11761, 11762,10586, 10585,11763, 11764], & 
& edgecnc=[3305,3306,2717,3307,3308,3309,2721,3310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(460),elname="xbrick",eltype="xbrick",typekey=460) 

        call prepare(lib_xbrick(461),key=461, & 
& nodecnc=[2204,2194,2199,170,4776,4766,4771,2742,11765, 11766,11767, 11768,11769, 11770,11771, 11772 & 
& ,11773, 11774,11775, 11776,11777, 11778,11779, 11780], & 
& edgecnc=[3311,3312,3313,3314,3315,3316,3317,3318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(461),elname="xbrick",eltype="xbrick",typekey=461) 

        call prepare(lib_xbrick(462),key=462, & 
& nodecnc=[2439,2122,188,2167,5011,4694,2760,4739,11781, 11782,11783, 11784,11785, 11786,11787, 11788 & 
& ,11789, 11790,11791, 11792,11793, 11794,11795, 11796], & 
& edgecnc=[3319,3320,3321,3322,3323,3324,3325,3326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(462),elname="xbrick",eltype="xbrick",typekey=462) 

        call prepare(lib_xbrick(463),key=463, & 
& nodecnc=[173,1868,1898,1737,2745,4440,4470,4309,7480, 7479,7492, 7491,11797, 11798,11799, 11800,7488 & 
& , 7487,7500, 7499,11801, 11802,11803, 11804], & 
& edgecnc=[1168,1174,3327,3328,1172,1178,3329,3330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(463),elname="xbrick",eltype="xbrick",typekey=463) 

        call prepare(lib_xbrick(464),key=464, & 
& nodecnc=[2490,2078,2143,2170,5062,4650,4715,4742,10662, 10661,11805, 11806,11807, 11808,11809, 11810 & 
& ,10670, 10669,11811, 11812,11813, 11814,11815, 11816], & 
& edgecnc=[2759,3331,3332,3333,2763,3334,3335,3336], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(464),elname="xbrick",eltype="xbrick",typekey=464) 

        call prepare(lib_xbrick(465),key=465, & 
& nodecnc=[2149,2172,2181,2169,4721,4744,4753,4741,11817, 11818,11819, 11820,11821, 11822,10564, 10563 & 
& ,11823, 11824,11825, 11826,11827, 11828,10570, 10569], & 
& edgecnc=[3337,3338,3339,2710,3340,3341,3342,2713], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(465),elname="xbrick",eltype="xbrick",typekey=465) 

        call prepare(lib_xbrick(466),key=466, & 
& nodecnc=[993,1027,1002,986,3565,3599,3574,3558,11829, 11830,11831, 11832,11833, 11834,11835, 11836,11837 & 
& , 11838,11839, 11840,11841, 11842,11843, 11844], & 
& edgecnc=[3343,3344,3345,3346,3347,3348,3349,3350], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(466),elname="xbrick",eltype="xbrick",typekey=466) 

        call prepare(lib_xbrick(467),key=467, & 
& nodecnc=[954,2536,892,913,3526,5108,3464,3485,11845, 11846,11847, 11848,11849, 11850,11851, 11852,11853 & 
& , 11854,11855, 11856,11857, 11858,11859, 11860], & 
& edgecnc=[3351,3352,3353,3354,3355,3356,3357,3358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(467),elname="xbrick",eltype="xbrick",typekey=467) 

        call prepare(lib_xbrick(468),key=468, & 
& nodecnc=[1243,1250,1230,1225,3815,3822,3802,3797,11861, 11862,11863, 11864,11090, 11089,11865, 11866 & 
& ,11867, 11868,11869, 11870,11098, 11097,11871, 11872], & 
& edgecnc=[3359,3360,2973,3361,3362,3363,2977,3364], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(468),elname="xbrick",eltype="xbrick",typekey=468) 

        call prepare(lib_xbrick(469),key=469, & 
& nodecnc=[1418,1412,181,1407,3990,3984,2753,3979,10832, 10831,11873, 11874,11875, 11876,7204, 7203,10840 & 
& , 10839,11877, 11878,11879, 11880,7212, 7211], & 
& edgecnc=[2844,3365,3366,1030,2848,3367,3368,1034], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(469),elname="xbrick",eltype="xbrick",typekey=469) 

        call prepare(lib_xbrick(470),key=470, & 
& nodecnc=[1731,182,1610,1666,4303,2754,4182,4238,11881, 11882,11883, 11884,11885, 11886,11887, 11888 & 
& ,11889, 11890,11891, 11892,11893, 11894,11895, 11896], & 
& edgecnc=[3369,3370,3371,3372,3373,3374,3375,3376], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(470),elname="xbrick",eltype="xbrick",typekey=470) 

        call prepare(lib_xbrick(471),key=471, & 
& nodecnc=[2159,2186,2171,2150,4731,4758,4743,4722,11897, 11898,11899, 11900,7906, 7905,11901, 11902,11903 & 
& , 11904,11905, 11906,7914, 7913,11907, 11908], & 
& edgecnc=[3377,3378,1381,3379,3380,3381,1385,3382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(471),elname="xbrick",eltype="xbrick",typekey=471) 

        call prepare(lib_xbrick(472),key=472, & 
& nodecnc=[2185,2325,221,2192,4757,4897,2793,4764,11909, 11910,11911, 11912,11913, 11914,7084, 7083,11915 & 
& , 11916,11917, 11918,11919, 11920,7092, 7091], & 
& edgecnc=[3383,3384,3385,970,3386,3387,3388,974], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(472),elname="xbrick",eltype="xbrick",typekey=472) 

        call prepare(lib_xbrick(473),key=473, & 
& nodecnc=[2145,2090,2354,222,4717,4662,4926,2794,11921, 11922,11923, 11924,11925, 11926,6894, 6893,11927 & 
& , 11928,11929, 11930,11931, 11932,6902, 6901], & 
& edgecnc=[3389,3390,3391,875,3392,3393,3394,879], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(473),elname="xbrick",eltype="xbrick",typekey=473) 

        call prepare(lib_xbrick(474),key=474, & 
& nodecnc=[187,2437,2163,2147,2759,5009,4735,4719,11933, 11934,7070, 7069,11935, 11936,11937, 11938,11939 & 
& , 11940,7078, 7077,11941, 11942,11943, 11944], & 
& edgecnc=[3395,963,3396,3397,3398,967,3399,3400], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(474),elname="xbrick",eltype="xbrick",typekey=474) 

        call prepare(lib_xbrick(475),key=475, & 
& nodecnc=[2049,172,1868,1925,4621,2744,4440,4497,11945, 11946,7494, 7493,7478, 7477,6912, 6911,11947 & 
& , 11948,7502, 7501,7486, 7485,6920, 6919], & 
& edgecnc=[3401,1175,1167,884,3402,1179,1171,888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(475),elname="xbrick",eltype="xbrick",typekey=475) 

        call prepare(lib_xbrick(476),key=476, & 
& nodecnc=[161,2524,1307,1264,2733,5096,3879,3836,11949, 11950,11951, 11952,11953, 11954,11955, 11956 & 
& ,11957, 11958,11959, 11960,11961, 11962,11963, 11964], & 
& edgecnc=[3403,3404,3405,3406,3407,3408,3409,3410], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(476),elname="xbrick",eltype="xbrick",typekey=476) 

        call prepare(lib_xbrick(477),key=477, & 
& nodecnc=[2127,1952,1891,287,4699,4524,4463,2859,11965, 11966,11967, 11968,11969, 11970,11971, 11972 & 
& ,11973, 11974,11975, 11976,11977, 11978,11979, 11980], & 
& edgecnc=[3411,3412,3413,3414,3415,3416,3417,3418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(477),elname="xbrick",eltype="xbrick",typekey=477) 

        call prepare(lib_xbrick(478),key=478, & 
& nodecnc=[1952,2084,196,2032,4524,4656,2768,4604,11981, 11982,11983, 11984,11985, 11986,11987, 11988 & 
& ,11989, 11990,11991, 11992,11993, 11994,11995, 11996], & 
& edgecnc=[3419,3420,3421,3422,3423,3424,3425,3426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(478),elname="xbrick",eltype="xbrick",typekey=478) 

        call prepare(lib_xbrick(479),key=479, & 
& nodecnc=[1863,2462,2522,2449,4435,5034,5094,5021,11997, 11998,11999, 12000,5670, 5669,12001, 12002,12003 & 
& , 12004,12005, 12006,5678, 5677,12007, 12008], & 
& edgecnc=[3427,3428,263,3429,3430,3431,267,3432], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(479),elname="xbrick",eltype="xbrick",typekey=479) 

        call prepare(lib_xbrick(480),key=480, & 
& nodecnc=[1024,1070,1011,1012,3596,3642,3583,3584,12009, 12010,12011, 12012,6390, 6389,12013, 12014,12015 & 
& , 12016,12017, 12018,6398, 6397,12019, 12020], & 
& edgecnc=[3433,3434,623,3435,3436,3437,627,3438], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(480),elname="xbrick",eltype="xbrick",typekey=480) 

        call prepare(lib_xbrick(481),key=481, & 
& nodecnc=[1616,1663,1206,1122,4188,4235,3778,3694,10406, 10405,12021, 12022,10394, 10393,12023, 12024 & 
& ,10414, 10413,12025, 12026,10402, 10401,12027, 12028], & 
& edgecnc=[2631,3439,2625,3440,2635,3441,2629,3442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(481),elname="xbrick",eltype="xbrick",typekey=481) 

        call prepare(lib_xbrick(482),key=482, & 
& nodecnc=[643,2267,2310,2265,3215,4839,4882,4837,12029, 12030,12031, 12032,12033, 12034,12035, 12036 & 
& ,12037, 12038,12039, 12040,12041, 12042,12043, 12044], & 
& edgecnc=[3443,3444,3445,3446,3447,3448,3449,3450], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(482),elname="xbrick",eltype="xbrick",typekey=482) 

        call prepare(lib_xbrick(483),key=483, & 
& nodecnc=[2395,1105,2443,2442,4967,3677,5015,5014,12045, 12046,12047, 12048,12049, 12050,12051, 12052 & 
& ,12053, 12054,12055, 12056,12057, 12058,12059, 12060], & 
& edgecnc=[3451,3452,3453,3454,3455,3456,3457,3458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(483),elname="xbrick",eltype="xbrick",typekey=483) 

        call prepare(lib_xbrick(484),key=484, & 
& nodecnc=[800,856,834,2425,3372,3428,3406,4997,12061, 12062,12063, 12064,12065, 12066,12067, 12068,12069 & 
& , 12070,12071, 12072,12073, 12074,12075, 12076], & 
& edgecnc=[3459,3460,3461,3462,3463,3464,3465,3466], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(484),elname="xbrick",eltype="xbrick",typekey=484) 

        call prepare(lib_xbrick(485),key=485, & 
& nodecnc=[1133,1085,1079,1106,3705,3657,3651,3678,12077, 12078,8472, 8471,12079, 12080,12081, 12082,12083 & 
& , 12084,8480, 8479,12085, 12086,12087, 12088], & 
& edgecnc=[3467,1664,3468,3469,3470,1668,3471,3472], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(485),elname="xbrick",eltype="xbrick",typekey=485) 

        call prepare(lib_xbrick(486),key=486, & 
& nodecnc=[1297,1318,1308,2390,3869,3890,3880,4962,12089, 12090,12091, 12092,12093, 12094,12095, 12096 & 
& ,12097, 12098,12099, 12100,12101, 12102,12103, 12104], & 
& edgecnc=[3473,3474,3475,3476,3477,3478,3479,3480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(486),elname="xbrick",eltype="xbrick",typekey=486) 

        call prepare(lib_xbrick(487),key=487, & 
& nodecnc=[1336,1347,1352,213,3908,3919,3924,2785,11682, 11681,11672, 11671,12105, 12106,12107, 12108 & 
& ,11690, 11689,11680, 11679,12109, 12110,12111, 12112], & 
& edgecnc=[3269,3264,3481,3482,3273,3268,3483,3484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(487),elname="xbrick",eltype="xbrick",typekey=487) 

        call prepare(lib_xbrick(488),key=488, & 
& nodecnc=[1298,1321,1371,1335,3870,3893,3943,3907,12113, 12114,11580, 11579,12115, 12116,12117, 12118 & 
& ,12119, 12120,11588, 11587,12121, 12122,12123, 12124], & 
& edgecnc=[3485,3218,3486,3487,3488,3222,3489,3490], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(488),elname="xbrick",eltype="xbrick",typekey=488) 

        call prepare(lib_xbrick(489),key=489, & 
& nodecnc=[183,1809,1731,1666,2755,4381,4303,4238,12125, 12126,12127, 12128,11888, 11887,7968, 7967,12129 & 
& , 12130,12131, 12132,11896, 11895,7976, 7975], & 
& edgecnc=[3491,3492,3372,1412,3493,3494,3376,1416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(489),elname="xbrick",eltype="xbrick",typekey=489) 

        call prepare(lib_xbrick(490),key=490, & 
& nodecnc=[219,2408,2082,2031,2791,4980,4654,4603,12133, 12134,12135, 12136,7920, 7919,10602, 10601,12137 & 
& , 12138,12139, 12140,7928, 7927,10610, 10609], & 
& edgecnc=[3495,3496,1388,2729,3497,3498,1392,2733], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(490),elname="xbrick",eltype="xbrick",typekey=490) 

        call prepare(lib_xbrick(491),key=491, & 
& nodecnc=[2016,2284,1950,2035,4588,4856,4522,4607,12141, 12142,12143, 12144,12145, 12146,12147, 12148 & 
& ,12149, 12150,12151, 12152,12153, 12154,12155, 12156], & 
& edgecnc=[3499,3500,3501,3502,3503,3504,3505,3506], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(491),elname="xbrick",eltype="xbrick",typekey=491) 

        call prepare(lib_xbrick(492),key=492, & 
& nodecnc=[2133,261,2115,2160,4705,2833,4687,4732,12157, 12158,12159, 12160,12161, 12162,12163, 12164 & 
& ,12165, 12166,12167, 12168,12169, 12170,12171, 12172], & 
& edgecnc=[3507,3508,3509,3510,3511,3512,3513,3514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(492),elname="xbrick",eltype="xbrick",typekey=492) 

        call prepare(lib_xbrick(493),key=493, & 
& nodecnc=[2523,1962,1817,1883,5095,4534,4389,4455,8982, 8981,12173, 12174,12175, 12176,12177, 12178,8990 & 
& , 8989,12179, 12180,12181, 12182,12183, 12184], & 
& edgecnc=[1919,3515,3516,3517,1923,3518,3519,3520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(493),elname="xbrick",eltype="xbrick",typekey=493) 

        call prepare(lib_xbrick(494),key=494, & 
& nodecnc=[1825,1763,1711,226,4397,4335,4283,2798,12185, 12186,7430, 7429,12187, 12188,12189, 12190,12191 & 
& , 12192,7438, 7437,12193, 12194,12195, 12196], & 
& edgecnc=[3521,1143,3522,3523,3524,1147,3525,3526], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(494),elname="xbrick",eltype="xbrick",typekey=494) 

        call prepare(lib_xbrick(495),key=495, & 
& nodecnc=[2002,1758,2227,1902,4574,4330,4799,4474,12197, 12198,6802, 6801,12199, 12200,12201, 12202,12203 & 
& , 12204,6810, 6809,12205, 12206,12207, 12208], & 
& edgecnc=[3527,829,3528,3529,3530,833,3531,3532], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(495),elname="xbrick",eltype="xbrick",typekey=495) 

        call prepare(lib_xbrick(496),key=496, & 
& nodecnc=[1810,1787,1675,232,4382,4359,4247,2804,12209, 12210,7302, 7301,7354, 7353,12211, 12212,12213 & 
& , 12214,7310, 7309,7362, 7361,12215, 12216], & 
& edgecnc=[3533,1079,1105,3534,3535,1083,1109,3536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(496),elname="xbrick",eltype="xbrick",typekey=496) 

        call prepare(lib_xbrick(497),key=497, & 
& nodecnc=[1476,1717,234,1483,4048,4289,2806,4055,11642, 11641,12217, 12218,12219, 12220,12221, 12222 & 
& ,11648, 11647,12223, 12224,12225, 12226,12227, 12228], & 
& edgecnc=[3249,3537,3538,3539,3252,3540,3541,3542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(497),elname="xbrick",eltype="xbrick",typekey=497) 

        call prepare(lib_xbrick(498),key=498, & 
& nodecnc=[1483,1477,409,42,4055,4049,2981,2614,12229, 12230,12231, 12232,12233, 12234,12235, 12236,12237 & 
& , 12238,12239, 12240,12241, 12242,12243, 12244], & 
& edgecnc=[3543,3544,3545,3546,3547,3548,3549,3550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(498),elname="xbrick",eltype="xbrick",typekey=498) 

        call prepare(lib_xbrick(499),key=499, & 
& nodecnc=[1455,268,2291,1453,4027,2840,4863,4025,12245, 12246,12247, 12248,12249, 12250,12251, 12252 & 
& ,12253, 12254,12255, 12256,12257, 12258,12259, 12260], & 
& edgecnc=[3551,3552,3553,3554,3555,3556,3557,3558], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(499),elname="xbrick",eltype="xbrick",typekey=499) 

        call prepare(lib_xbrick(500),key=500, & 
& nodecnc=[1795,1788,1861,1703,4367,4360,4433,4275,12261, 12262,12263, 12264,7344, 7343,12265, 12266,12267 & 
& , 12268,12269, 12270,7352, 7351,12271, 12272], & 
& edgecnc=[3559,3560,1100,3561,3562,3563,1104,3564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(500),elname="xbrick",eltype="xbrick",typekey=500) 

        call prepare(lib_xbrick(501),key=501, & 
& nodecnc=[1406,1409,1411,1415,3978,3981,3983,3987,11590, 11589,12273, 12274,12275, 12276,12277, 12278 & 
& ,11598, 11597,12279, 12280,12281, 12282,12283, 12284], & 
& edgecnc=[3223,3565,3566,3567,3227,3568,3569,3570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(501),elname="xbrick",eltype="xbrick",typekey=501) 

        call prepare(lib_xbrick(502),key=502, & 
& nodecnc=[1400,1389,1414,1409,3972,3961,3986,3981,12285, 12286,12287, 12288,12289, 12290,11596, 11595 & 
& ,12291, 12292,12293, 12294,12295, 12296,11604, 11603], & 
& edgecnc=[3571,3572,3573,3226,3574,3575,3576,3230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(502),elname="xbrick",eltype="xbrick",typekey=502) 

        call prepare(lib_xbrick(503),key=503, & 
& nodecnc=[1388,235,1434,1435,3960,2807,4006,4007,10998, 10997,12297, 12298,12299, 12300,11608, 11607 & 
& ,11006, 11005,12301, 12302,12303, 12304,11616, 11615], & 
& edgecnc=[2927,3577,3578,3232,2931,3579,3580,3236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(503),elname="xbrick",eltype="xbrick",typekey=503) 

        call prepare(lib_xbrick(504),key=504, & 
& nodecnc=[1029,1104,1030,949,3601,3676,3602,3521,12305, 12306,12307, 12308,12309, 12310,12311, 12312 & 
& ,12313, 12314,12315, 12316,12317, 12318,12319, 12320], & 
& edgecnc=[3581,3582,3583,3584,3585,3586,3587,3588], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(504),elname="xbrick",eltype="xbrick",typekey=504) 

        call prepare(lib_xbrick(505),key=505, & 
& nodecnc=[2224,275,881,897,4796,2847,3453,3469,12321, 12322,12323, 12324,12325, 12326,12327, 12328,12329 & 
& , 12330,12331, 12332,12333, 12334,12335, 12336], & 
& edgecnc=[3589,3590,3591,3592,3593,3594,3595,3596], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(505),elname="xbrick",eltype="xbrick",typekey=505) 

        call prepare(lib_xbrick(506),key=506, & 
& nodecnc=[947,944,1042,977,3519,3516,3614,3549,12337, 12338,12339, 12340,12341, 12342,12343, 12344,12345 & 
& , 12346,12347, 12348,12349, 12350,12351, 12352], & 
& edgecnc=[3597,3598,3599,3600,3601,3602,3603,3604], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(506),elname="xbrick",eltype="xbrick",typekey=506) 

        call prepare(lib_xbrick(507),key=507, & 
& nodecnc=[2225,1040,1013,1042,4797,3612,3585,3614,12353, 12354,12355, 12356,12357, 12358,12359, 12360 & 
& ,12361, 12362,12363, 12364,12365, 12366,12367, 12368], & 
& edgecnc=[3605,3606,3607,3608,3609,3610,3611,3612], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(507),elname="xbrick",eltype="xbrick",typekey=507) 

        call prepare(lib_xbrick(508),key=508, & 
& nodecnc=[1198,1815,1127,1169,3770,4387,3699,3741,12369, 12370,12371, 12372,12373, 12374,10426, 10425 & 
& ,12375, 12376,12377, 12378,12379, 12380,10434, 10433], & 
& edgecnc=[3613,3614,3615,2641,3616,3617,3618,2645], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(508),elname="xbrick",eltype="xbrick",typekey=508) 

        call prepare(lib_xbrick(509),key=509, & 
& nodecnc=[1098,1129,1169,1127,3670,3701,3741,3699,12381, 12382,12383, 12384,12374, 12373,12385, 12386 & 
& ,12387, 12388,12389, 12390,12380, 12379,12391, 12392], & 
& edgecnc=[3619,3620,3615,3621,3622,3623,3618,3624], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(509),elname="xbrick",eltype="xbrick",typekey=509) 

        call prepare(lib_xbrick(510),key=510, & 
& nodecnc=[1856,1792,1690,249,4428,4364,4262,2821,12393, 12394,9256, 9255,5740, 5739,10286, 10285,12395 & 
& , 12396,9262, 9261,5748, 5747,10292, 10291], & 
& edgecnc=[3625,2056,298,2571,3626,2059,302,2574], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(510),elname="xbrick",eltype="xbrick",typekey=510) 

        call prepare(lib_xbrick(511),key=511, & 
& nodecnc=[1838,251,1772,1760,4410,2823,4344,4332,5772, 5771,12397, 12398,12399, 12400,12401, 12402,5780 & 
& , 5779,12403, 12404,12405, 12406,12407, 12408], & 
& edgecnc=[314,3627,3628,3629,318,3630,3631,3632], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(511),elname="xbrick",eltype="xbrick",typekey=511) 

        call prepare(lib_xbrick(512),key=512, & 
& nodecnc=[1823,1760,1772,252,4395,4332,4344,2824,12409, 12410,12400, 12399,12411, 12412,12413, 12414 & 
& ,12415, 12416,12406, 12405,12417, 12418,12419, 12420], & 
& edgecnc=[3633,3628,3634,3635,3636,3631,3637,3638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(512),elname="xbrick",eltype="xbrick",typekey=512) 

        call prepare(lib_xbrick(513),key=513, & 
& nodecnc=[377,376,1595,1480,2949,2948,4167,4052,12421, 12422,12423, 12424,12425, 12426,12427, 12428,12429 & 
& , 12430,12431, 12432,12433, 12434,12435, 12436], & 
& edgecnc=[3639,3640,3641,3642,3643,3644,3645,3646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(513),elname="xbrick",eltype="xbrick",typekey=513) 

        call prepare(lib_xbrick(514),key=514, & 
& nodecnc=[256,1595,1479,1725,2828,4167,4051,4297,12437, 12438,12439, 12440,12441, 12442,6044, 6043,12443 & 
& , 12444,12445, 12446,12447, 12448,6052, 6051], & 
& edgecnc=[3647,3648,3649,450,3650,3651,3652,454], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(514),elname="xbrick",eltype="xbrick",typekey=514) 

        call prepare(lib_xbrick(515),key=515, & 
& nodecnc=[90,1545,1519,496,2662,4117,4091,3068,6186, 6185,12449, 12450,12451, 12452,12453, 12454,6194 & 
& , 6193,12455, 12456,12457, 12458,12459, 12460], & 
& edgecnc=[521,3653,3654,3655,525,3656,3657,3658], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(515),elname="xbrick",eltype="xbrick",typekey=515) 

        call prepare(lib_xbrick(516),key=516, & 
& nodecnc=[2284,1967,1922,260,4856,4539,4494,2832,12461, 12462,12463, 12464,12465, 12466,12467, 12468 & 
& ,12469, 12470,12471, 12472,12473, 12474,12475, 12476], & 
& edgecnc=[3659,3660,3661,3662,3663,3664,3665,3666], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(516),elname="xbrick",eltype="xbrick",typekey=516) 

        call prepare(lib_xbrick(517),key=517, & 
& nodecnc=[1853,1917,1704,1805,4425,4489,4276,4377,12477, 12478,12479, 12480,7282, 7281,12481, 12482,12483 & 
& , 12484,12485, 12486,7290, 7289,12487, 12488], & 
& edgecnc=[3667,3668,1069,3669,3670,3671,1073,3672], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(517),elname="xbrick",eltype="xbrick",typekey=517) 

        call prepare(lib_xbrick(518),key=518, & 
& nodecnc=[838,2353,805,2460,3410,4925,3377,5032,12489, 12490,12491, 12492,12493, 12494,12495, 12496,12497 & 
& , 12498,12499, 12500,12501, 12502,12503, 12504], & 
& edgecnc=[3673,3674,3675,3676,3677,3678,3679,3680], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(518),elname="xbrick",eltype="xbrick",typekey=518) 

        call prepare(lib_xbrick(519),key=519, & 
& nodecnc=[904,860,793,833,3476,3432,3365,3405,12505, 12506,12507, 12508,12509, 12510,6552, 6551,12511 & 
& , 12512,12513, 12514,12515, 12516,6560, 6559], & 
& edgecnc=[3681,3682,3683,704,3684,3685,3686,708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(519),elname="xbrick",eltype="xbrick",typekey=519) 

        call prepare(lib_xbrick(520),key=520, & 
& nodecnc=[1620,278,1794,1720,4192,2850,4366,4292,12517, 12518,12519, 12520,12521, 12522,9808, 9807,12523 & 
& , 12524,12525, 12526,12527, 12528,9816, 9815], & 
& edgecnc=[3687,3688,3689,2332,3690,3691,3692,2336], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(520),elname="xbrick",eltype="xbrick",typekey=520) 

        call prepare(lib_xbrick(521),key=521, & 
& nodecnc=[1992,1746,1887,1914,4564,4318,4459,4486,12529, 12530,12531, 12532,12533, 12534,9950, 9949,12535 & 
& , 12536,12537, 12538,12539, 12540,9958, 9957], & 
& edgecnc=[3693,3694,3695,2403,3696,3697,3698,2407], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(521),elname="xbrick",eltype="xbrick",typekey=521) 

        call prepare(lib_xbrick(522),key=522, & 
& nodecnc=[1757,2279,1799,1715,4329,4851,4371,4287,12541, 12542,12543, 12544,6338, 6337,5706, 5705,12545 & 
& , 12546,12547, 12548,6344, 6343,5714, 5713], & 
& edgecnc=[3699,3700,597,281,3701,3702,600,285], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(522),elname="xbrick",eltype="xbrick",typekey=522) 

        call prepare(lib_xbrick(523),key=523, & 
& nodecnc=[1963,2014,2039,2008,4535,4586,4611,4580,6144, 6143,12549, 12550,12551, 12552,12553, 12554,6152 & 
& , 6151,12555, 12556,12557, 12558,12559, 12560], & 
& edgecnc=[500,3703,3704,3705,504,3706,3707,3708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(523),elname="xbrick",eltype="xbrick",typekey=523) 

        call prepare(lib_xbrick(524),key=524, & 
& nodecnc=[1945,288,1871,1901,4517,2860,4443,4473,12561, 12562,6104, 6103,12563, 12564,12565, 12566,12567 & 
& , 12568,6110, 6109,12569, 12570,12571, 12572], & 
& edgecnc=[3709,480,3710,3711,3712,483,3713,3714], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(524),elname="xbrick",eltype="xbrick",typekey=524) 

        call prepare(lib_xbrick(525),key=525, & 
& nodecnc=[2328,1963,1920,192,4900,4535,4492,2764,6138, 6137,12573, 12574,12575, 12576,12577, 12578,6146 & 
& , 6145,12579, 12580,12581, 12582,12583, 12584], & 
& edgecnc=[497,3715,3716,3717,501,3718,3719,3720], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(525),elname="xbrick",eltype="xbrick",typekey=525) 

        call prepare(lib_xbrick(526),key=526, & 
& nodecnc=[1831,255,1714,1767,4403,2827,4286,4339,12585, 12586,12587, 12588,6060, 6059,12589, 12590,12591 & 
& , 12592,12593, 12594,6066, 6065,12595, 12596], & 
& edgecnc=[3721,3722,458,3723,3724,3725,461,3726], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(526),elname="xbrick",eltype="xbrick",typekey=526) 

        call prepare(lib_xbrick(527),key=527, & 
& nodecnc=[1846,291,1689,1768,4418,2863,4261,4340,12597, 12598,6202, 6201,6156, 6155,12599, 12600,12601 & 
& , 12602,6210, 6209,6164, 6163,12603, 12604], & 
& edgecnc=[3727,529,506,3728,3729,533,510,3730], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(527),elname="xbrick",eltype="xbrick",typekey=527) 

        call prepare(lib_xbrick(528),key=528, & 
& nodecnc=[1899,1870,328,1736,4471,4442,2900,4308,7026, 7025,12605, 12606,12607, 12608,12609, 12610,7034 & 
& , 7033,12611, 12612,12613, 12614,12615, 12616], & 
& edgecnc=[941,3731,3732,3733,945,3734,3735,3736], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(528),elname="xbrick",eltype="xbrick",typekey=528) 

        call prepare(lib_xbrick(529),key=529, & 
& nodecnc=[1829,327,1713,1766,4401,2899,4285,4338,12617, 12618,6980, 6979,6944, 6943,12619, 12620,12621 & 
& , 12622,6988, 6987,6952, 6951,12623, 12624], & 
& edgecnc=[3737,918,900,3738,3739,922,904,3740], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(529),elname="xbrick",eltype="xbrick",typekey=529) 

        call prepare(lib_xbrick(530),key=530, & 
& nodecnc=[2095,2004,294,2048,4667,4576,2866,4620,7856, 7855,7846, 7845,12625, 12626,12627, 12628,7864 & 
& , 7863,7852, 7851,12629, 12630,12631, 12632], & 
& edgecnc=[1356,1351,3741,3742,1360,1354,3743,3744], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(530),elname="xbrick",eltype="xbrick",typekey=530) 

        call prepare(lib_xbrick(531),key=531, & 
& nodecnc=[2125,1953,1890,295,4697,4525,4462,2867,12633, 12634,12635, 12636,12637, 12638,12639, 12640 & 
& ,12641, 12642,12643, 12644,12645, 12646,12647, 12648], & 
& edgecnc=[3745,3746,3747,3748,3749,3750,3751,3752], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(531),elname="xbrick",eltype="xbrick",typekey=531) 

        call prepare(lib_xbrick(532),key=532, & 
& nodecnc=[2078,1958,296,2080,4650,4530,2868,4652,12649, 12650,12651, 12652,12653, 12654,12655, 12656 & 
& ,12657, 12658,12659, 12660,12661, 12662,12663, 12664], & 
& edgecnc=[3753,3754,3755,3756,3757,3758,3759,3760], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(532),elname="xbrick",eltype="xbrick",typekey=532) 

        call prepare(lib_xbrick(533),key=533, & 
& nodecnc=[1851,332,1727,1779,4423,2904,4299,4351,10720, 10719,8108, 8107,8024, 8023,12665, 12666,10728 & 
& , 10727,8116, 8115,8032, 8031,12667, 12668], & 
& edgecnc=[2788,1482,1440,3761,2792,1486,1444,3762], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(533),elname="xbrick",eltype="xbrick",typekey=533) 

        call prepare(lib_xbrick(534),key=534, & 
& nodecnc=[1855,333,1693,1789,4427,2905,4265,4361,12669, 12670,8124, 8123,10716, 10715,12671, 12672,12673 & 
& , 12674,8132, 8131,10724, 10723,12675, 12676], & 
& edgecnc=[3763,1490,2786,3764,3765,1494,2790,3766], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(534),elname="xbrick",eltype="xbrick",typekey=534) 

        call prepare(lib_xbrick(535),key=535, & 
& nodecnc=[1855,1791,1691,333,4427,4363,4263,2905,12677, 12678,12679, 12680,12681, 12682,12670, 12669 & 
& ,12683, 12684,12685, 12686,12687, 12688,12674, 12673], & 
& edgecnc=[3767,3768,3769,3763,3770,3771,3772,3765], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(535),elname="xbrick",eltype="xbrick",typekey=535) 

        call prepare(lib_xbrick(536),key=536, & 
& nodecnc=[1848,334,1691,1791,4420,2906,4263,4363,8184, 8183,8136, 8135,12680, 12679,12689, 12690,8192 & 
& , 8191,8144, 8143,12686, 12685,12691, 12692], & 
& edgecnc=[1520,1496,3768,3773,1524,1500,3771,3774], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(536),elname="xbrick",eltype="xbrick",typekey=536) 

        call prepare(lib_xbrick(537),key=537, & 
& nodecnc=[1937,1638,302,1658,4509,4210,2874,4230,12693, 12694,12695, 12696,12697, 12698,12699, 12700 & 
& ,12701, 12702,12703, 12704,12705, 12706,12707, 12708], & 
& edgecnc=[3775,3776,3777,3778,3779,3780,3781,3782], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(537),elname="xbrick",eltype="xbrick",typekey=537) 

        call prepare(lib_xbrick(538),key=538, & 
& nodecnc=[1096,1126,1121,1064,3668,3698,3693,3636,12709, 12710,12711, 12712,8288, 8287,8268, 8267,12713 & 
& , 12714,12715, 12716,8296, 8295,8276, 8275], & 
& edgecnc=[3783,3784,1572,1562,3785,3786,1576,1566], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(538),elname="xbrick",eltype="xbrick",typekey=538) 

        call prepare(lib_xbrick(539),key=539, & 
& nodecnc=[1221,1227,1248,338,3793,3799,3820,2910,12717, 12718,8534, 8533,12719, 12720,12721, 12722,12723 & 
& , 12724,8542, 8541,12725, 12726,12727, 12728], & 
& edgecnc=[3787,1695,3788,3789,3790,1699,3791,3792], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(539),elname="xbrick",eltype="xbrick",typekey=539) 

        call prepare(lib_xbrick(540),key=540, & 
& nodecnc=[2307,138,848,859,4879,2710,3420,3431,12729, 12730,12731, 12732,12733, 12734,12735, 12736,12737 & 
& , 12738,12739, 12740,12741, 12742,12743, 12744], & 
& edgecnc=[3793,3794,3795,3796,3797,3798,3799,3800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(540),elname="xbrick",eltype="xbrick",typekey=540) 

        call prepare(lib_xbrick(541),key=541, & 
& nodecnc=[844,785,746,771,3416,3357,3318,3343,12745, 12746,12747, 12748,12749, 12750,12751, 12752,12753 & 
& , 12754,12755, 12756,12757, 12758,12759, 12760], & 
& edgecnc=[3801,3802,3803,3804,3805,3806,3807,3808], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(541),elname="xbrick",eltype="xbrick",typekey=541) 

        call prepare(lib_xbrick(542),key=542, & 
& nodecnc=[1273,1252,1247,312,3845,3824,3819,2884,12761, 12762,12763, 12764,12765, 12766,12767, 12768 & 
& ,12769, 12770,12771, 12772,12773, 12774,12775, 12776], & 
& edgecnc=[3809,3810,3811,3812,3813,3814,3815,3816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(542),elname="xbrick",eltype="xbrick",typekey=542) 

        call prepare(lib_xbrick(543),key=543, & 
& nodecnc=[1247,1234,1251,1253,3819,3806,3823,3825,12777, 12778,9886, 9885,12779, 12780,12781, 12782,12783 & 
& , 12784,9894, 9893,12785, 12786,12787, 12788], & 
& edgecnc=[3817,2371,3818,3819,3820,2375,3821,3822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(543),elname="xbrick",eltype="xbrick",typekey=543) 

        call prepare(lib_xbrick(544),key=544, & 
& nodecnc=[1272,1266,1278,313,3844,3838,3850,2885,9866, 9865,12789, 12790,12791, 12792,12793, 12794,9874 & 
& , 9873,12795, 12796,12797, 12798,12799, 12800], & 
& edgecnc=[2361,3823,3824,3825,2365,3826,3827,3828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(544),elname="xbrick",eltype="xbrick",typekey=544) 

        call prepare(lib_xbrick(545),key=545, & 
& nodecnc=[1349,314,1312,1342,3921,2886,3884,3914,12801, 12802,12803, 12804,12805, 12806,12807, 12808 & 
& ,12809, 12810,12811, 12812,12813, 12814,12815, 12816], & 
& edgecnc=[3829,3830,3831,3832,3833,3834,3835,3836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(545),elname="xbrick",eltype="xbrick",typekey=545) 

        call prepare(lib_xbrick(546),key=546, & 
& nodecnc=[351,1461,2260,1613,2923,4033,4832,4185,9532, 9531,12817, 12818,12819, 12820,12821, 12822,9540 & 
& , 9539,12823, 12824,12825, 12826,12827, 12828], & 
& edgecnc=[2194,3837,3838,3839,2198,3840,3841,3842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(546),elname="xbrick",eltype="xbrick",typekey=546) 

        call prepare(lib_xbrick(547),key=547, & 
& nodecnc=[1669,1654,316,2288,4241,4226,2888,4860,12829, 12830,12831, 12832,12833, 12834,12835, 12836 & 
& ,12837, 12838,12839, 12840,12841, 12842,12843, 12844], & 
& edgecnc=[3843,3844,3845,3846,3847,3848,3849,3850], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(547),elname="xbrick",eltype="xbrick",typekey=547) 

        call prepare(lib_xbrick(548),key=548, & 
& nodecnc=[1459,1449,1447,318,4031,4021,4019,2890,12845, 12846,9452, 9451,12847, 12848,12849, 12850,12851 & 
& , 12852,9460, 9459,12853, 12854,12855, 12856], & 
& edgecnc=[3851,2154,3852,3853,3854,2158,3855,3856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(548),elname="xbrick",eltype="xbrick",typekey=548) 

        call prepare(lib_xbrick(549),key=549, & 
& nodecnc=[1661,1635,1459,318,4233,4207,4031,2890,12857, 12858,12859, 12860,12850, 12849,12861, 12862 & 
& ,12863, 12864,12865, 12866,12856, 12855,12867, 12868], & 
& edgecnc=[3857,3858,3853,3859,3860,3861,3856,3862], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(549),elname="xbrick",eltype="xbrick",typekey=549) 

        call prepare(lib_xbrick(550),key=550, & 
& nodecnc=[2261,2263,1803,1749,4833,4835,4375,4321,12869, 12870,12871, 12872,12873, 12874,12875, 12876 & 
& ,12877, 12878,12879, 12880,12881, 12882,12883, 12884], & 
& edgecnc=[3863,3864,3865,3866,3867,3868,3869,3870], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(550),elname="xbrick",eltype="xbrick",typekey=550) 

        call prepare(lib_xbrick(551),key=551, & 
& nodecnc=[1803,1635,1661,1749,4375,4207,4233,4321,12885, 12886,12858, 12857,12887, 12888,12874, 12873 & 
& ,12889, 12890,12864, 12863,12891, 12892,12882, 12881], & 
& edgecnc=[3871,3857,3872,3865,3873,3860,3874,3869], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(551),elname="xbrick",eltype="xbrick",typekey=551) 

        call prepare(lib_xbrick(552),key=552, & 
& nodecnc=[2046,1951,1892,321,4618,4523,4464,2893,12893, 12894,12895, 12896,12897, 12898,12899, 12900 & 
& ,12901, 12902,12903, 12904,12905, 12906,12907, 12908], & 
& edgecnc=[3875,3876,3877,3878,3879,3880,3881,3882], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(552),elname="xbrick",eltype="xbrick",typekey=552) 

        call prepare(lib_xbrick(553),key=553, & 
& nodecnc=[2033,1966,1923,322,4605,4538,4495,2894,12909, 12910,12911, 12912,12913, 12914,12915, 12916 & 
& ,12917, 12918,12919, 12920,12921, 12922,12923, 12924], & 
& edgecnc=[3883,3884,3885,3886,3887,3888,3889,3890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(553),elname="xbrick",eltype="xbrick",typekey=553) 

        call prepare(lib_xbrick(554),key=554, & 
& nodecnc=[1222,2483,135,1236,3794,5055,2707,3808,12925, 12926,12927, 12928,12929, 12930,12931, 12932 & 
& ,12933, 12934,12935, 12936,12937, 12938,12939, 12940], & 
& edgecnc=[3891,3892,3893,3894,3895,3896,3897,3898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(554),elname="xbrick",eltype="xbrick",typekey=554) 

        call prepare(lib_xbrick(555),key=555, & 
& nodecnc=[2276,328,1870,1924,4848,2900,4442,4496,12941, 12942,12606, 12605,7844, 7843,12943, 12944,12945 & 
& , 12946,12612, 12611,7850, 7849,12947, 12948], & 
& edgecnc=[3899,3731,1350,3900,3901,3734,1353,3902], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(555),elname="xbrick",eltype="xbrick",typekey=555) 

        call prepare(lib_xbrick(556),key=556, & 
& nodecnc=[1691,1574,1537,333,4263,4146,4109,2905,8134, 8133,12949, 12950,8118, 8117,12682, 12681,8142 & 
& , 8141,12951, 12952,8126, 8125,12688, 12687], & 
& edgecnc=[1495,3903,1487,3769,1499,3904,1491,3772], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(556),elname="xbrick",eltype="xbrick",typekey=556) 

        call prepare(lib_xbrick(557),key=557, & 
& nodecnc=[975,1035,1025,1092,3547,3607,3597,3664,12953, 12954,12955, 12956,8688, 8687,12957, 12958,12959 & 
& , 12960,12961, 12962,8696, 8695,12963, 12964], & 
& edgecnc=[3905,3906,1772,3907,3908,3909,1776,3910], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(557),elname="xbrick",eltype="xbrick",typekey=557) 

        call prepare(lib_xbrick(558),key=558, & 
& nodecnc=[307,931,1050,955,2879,3503,3622,3527,12965, 12966,12967, 12968,12969, 12970,12971, 12972,12973 & 
& , 12974,12975, 12976,12977, 12978,12979, 12980], & 
& edgecnc=[3911,3912,3913,3914,3915,3916,3917,3918], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(558),elname="xbrick",eltype="xbrick",typekey=558) 

        call prepare(lib_xbrick(559),key=559, & 
& nodecnc=[1341,1327,1342,1312,3913,3899,3914,3884,12981, 12982,8900, 8899,12806, 12805,12983, 12984,12985 & 
& , 12986,8908, 8907,12814, 12813,12987, 12988], & 
& edgecnc=[3919,1878,3831,3920,3921,1882,3835,3922], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(559),elname="xbrick",eltype="xbrick",typekey=559) 

        call prepare(lib_xbrick(560),key=560, & 
& nodecnc=[2298,1626,1419,1413,4870,4198,3991,3985,12989, 12990,12991, 12992,12993, 12994,9518, 9517,12995 & 
& , 12996,12997, 12998,12999, 13000,9526, 9525], & 
& edgecnc=[3923,3924,3925,2187,3926,3927,3928,2191], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(560),elname="xbrick",eltype="xbrick",typekey=560) 

        call prepare(lib_xbrick(561),key=561, & 
& nodecnc=[1654,350,1668,1626,4226,2922,4240,4198,13001, 13002,13003, 13004,13005, 13006,13007, 13008 & 
& ,13009, 13010,13011, 13012,13013, 13014,13015, 13016], & 
& edgecnc=[3929,3930,3931,3932,3933,3934,3935,3936], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(561),elname="xbrick",eltype="xbrick",typekey=561) 

        call prepare(lib_xbrick(562),key=562, & 
& nodecnc=[1478,1498,78,473,4050,4070,2650,3045,13017, 13018,13019, 13020,13021, 13022,13023, 13024,13025 & 
& , 13026,13027, 13028,13029, 13030,13031, 13032], & 
& edgecnc=[3937,3938,3939,3940,3941,3942,3943,3944], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(562),elname="xbrick",eltype="xbrick",typekey=562) 

        call prepare(lib_xbrick(563),key=563, & 
& nodecnc=[1824,1761,1712,355,4396,4333,4284,2927,13033, 13034,9028, 9027,13035, 13036,13037, 13038,13039 & 
& , 13040,9034, 9033,13041, 13042,13043, 13044], & 
& edgecnc=[3945,1942,3946,3947,3948,1945,3949,3950], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(563),elname="xbrick",eltype="xbrick",typekey=563) 

        call prepare(lib_xbrick(564),key=564, & 
& nodecnc=[1679,357,1697,1781,4251,2929,4269,4353,13045, 13046,13047, 13048,5864, 5863,13049, 13050,13051 & 
& , 13052,13053, 13054,5872, 5871,13055, 13056], & 
& edgecnc=[3951,3952,360,3953,3954,3955,364,3956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(564),elname="xbrick",eltype="xbrick",typekey=564) 

        call prepare(lib_xbrick(565),key=565, & 
& nodecnc=[1801,1499,1474,1719,4373,4071,4046,4291,13057, 13058,9434, 9433,13059, 13060,13061, 13062,13063 & 
& , 13064,9442, 9441,13065, 13066,13067, 13068], & 
& edgecnc=[3957,2145,3958,3959,3960,2149,3961,3962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(565),elname="xbrick",eltype="xbrick",typekey=565) 

        call prepare(lib_xbrick(566),key=566, & 
& nodecnc=[1354,1363,349,1355,3926,3935,2921,3927,13069, 13070,13071, 13072,13073, 13074,13075, 13076 & 
& ,13077, 13078,13079, 13080,13081, 13082,13083, 13084], & 
& edgecnc=[3963,3964,3965,3966,3967,3968,3969,3970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(566),elname="xbrick",eltype="xbrick",typekey=566) 

        call prepare(lib_xbrick(567),key=567, & 
& nodecnc=[255,1481,1480,1714,2827,4053,4052,4286,13085, 13086,13087, 13088,13089, 13090,12588, 12587 & 
& ,13091, 13092,13093, 13094,13095, 13096,12594, 12593], & 
& edgecnc=[3971,3972,3973,3722,3974,3975,3976,3725], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(567),elname="xbrick",eltype="xbrick",typekey=567) 

        call prepare(lib_xbrick(568),key=568, & 
& nodecnc=[2221,1583,1543,1688,4793,4155,4115,4260,6170, 6169,9680, 9679,13097, 13098,13099, 13100,6178 & 
& , 6177,9688, 9687,13101, 13102,13103, 13104], & 
& edgecnc=[513,2268,3977,3978,517,2272,3979,3980], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(568),elname="xbrick",eltype="xbrick",typekey=568) 

        call prepare(lib_xbrick(569),key=569, & 
& nodecnc=[360,512,501,4,2932,3084,3073,2576,8866, 8865,5184, 5183,13105, 13106,13107, 13108,8874, 8873 & 
& ,5192, 5191,13109, 13110,13111, 13112], & 
& edgecnc=[1861,20,3981,3982,1865,24,3983,3984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(569),elname="xbrick",eltype="xbrick",typekey=569) 

        call prepare(lib_xbrick(570),key=570, & 
& nodecnc=[516,6,361,509,3088,2578,2933,3081,13113, 13114,13115, 13116,5194, 5193,13117, 13118,13119, 13120 & 
& ,13121, 13122,5202, 5201,13123, 13124], & 
& edgecnc=[3985,3986,25,3987,3988,3989,29,3990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(570),elname="xbrick",eltype="xbrick",typekey=570) 

        call prepare(lib_xbrick(571),key=571, & 
& nodecnc=[366,535,530,13,2938,3107,3102,2585,5318, 5317,13125, 13126,5342, 5341,13127, 13128,5326, 5325 & 
& ,13129, 13130,5350, 5349,13131, 13132], & 
& edgecnc=[87,3991,99,3992,91,3993,103,3994], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(571),elname="xbrick",eltype="xbrick",typekey=571) 

        call prepare(lib_xbrick(572),key=572, & 
& nodecnc=[528,372,21,527,3100,2944,2593,3099,13133, 13134,13135, 13136,5468, 5467,13137, 13138,13139 & 
& , 13140,13141, 13142,5474, 5473,13143, 13144], & 
& edgecnc=[3995,3996,162,3997,3998,3999,165,4000], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(572),elname="xbrick",eltype="xbrick",typekey=572) 

        call prepare(lib_xbrick(573),key=573, & 
& nodecnc=[372,528,522,22,2944,3100,3094,2594,13134, 13133,13145, 13146,5478, 5477,13147, 13148,13140 & 
& , 13139,13149, 13150,5486, 5485,13151, 13152], & 
& edgecnc=[3995,4001,167,4002,3998,4003,171,4004], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(573),elname="xbrick",eltype="xbrick",typekey=573) 

        call prepare(lib_xbrick(574),key=574, & 
& nodecnc=[373,499,498,24,2945,3071,3070,2596,5496, 5495,5510, 5509,13153, 13154,13155, 13156,5502, 5501 & 
& ,5518, 5517,13157, 13158,13159, 13160], & 
& edgecnc=[176,183,4005,4006,179,187,4007,4008], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(574),elname="xbrick",eltype="xbrick",typekey=574) 

        call prepare(lib_xbrick(575),key=575, & 
& nodecnc=[257,1596,2206,1519,2829,4168,4778,4091,13161, 13162,13163, 13164,13165, 13166,13167, 13168 & 
& ,13169, 13170,13171, 13172,13173, 13174,13175, 13176], & 
& edgecnc=[4009,4010,4011,4012,4013,4014,4015,4016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(575),elname="xbrick",eltype="xbrick",typekey=575) 

        call prepare(lib_xbrick(576),key=576, & 
& nodecnc=[1596,1479,25,375,4168,4051,2597,2947,13177, 13178,13179, 13180,13181, 13182,13183, 13184,13185 & 
& , 13186,13187, 13188,13189, 13190,13191, 13192], & 
& edgecnc=[4017,4018,4019,4020,4021,4022,4023,4024], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(576),elname="xbrick",eltype="xbrick",typekey=576) 

        call prepare(lib_xbrick(577),key=577, & 
& nodecnc=[26,377,1480,1481,2598,2949,4052,4053,13193, 13194,12428, 12427,13088, 13087,6260, 6259,13195 & 
& , 13196,12436, 12435,13094, 13093,6268, 6267], & 
& edgecnc=[4025,3642,3972,558,4026,3646,3975,562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(577),elname="xbrick",eltype="xbrick",typekey=577) 

        call prepare(lib_xbrick(578),key=578, & 
& nodecnc=[379,378,1523,1547,2951,2950,4095,4119,13197, 13198,6264, 6263,6278, 6277,13199, 13200,13201 & 
& , 13202,6272, 6271,6286, 6285,13203, 13204], & 
& edgecnc=[4027,560,567,4028,4029,564,571,4030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(578),elname="xbrick",eltype="xbrick",typekey=578) 

        call prepare(lib_xbrick(579),key=579, & 
& nodecnc=[380,27,1548,1521,2952,2599,4120,4093,13205, 13206,13207, 13208,13209, 13210,6238, 6237,13211 & 
& , 13212,13213, 13214,13215, 13216,6244, 6243], & 
& edgecnc=[4031,4032,4033,547,4034,4035,4036,550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(579),elname="xbrick",eltype="xbrick",typekey=579) 

        call prepare(lib_xbrick(580),key=580, & 
& nodecnc=[382,28,1550,1522,2954,2600,4122,4094,13217, 13218,13219, 13220,13221, 13222,5988, 5987,13223 & 
& , 13224,13225, 13226,13227, 13228,5996, 5995], & 
& edgecnc=[4037,4038,4039,422,4040,4041,4042,426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(580),elname="xbrick",eltype="xbrick",typekey=580) 

        call prepare(lib_xbrick(581),key=581, & 
& nodecnc=[252,1772,1517,1522,2824,4344,4089,4094,12412, 12411,13229, 13230,5982, 5981,13231, 13232,12418 & 
& , 12417,13233, 13234,5990, 5989,13235, 13236], & 
& edgecnc=[3634,4043,419,4044,3637,4045,423,4046], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(581),elname="xbrick",eltype="xbrick",typekey=581) 

        call prepare(lib_xbrick(582),key=582, & 
& nodecnc=[1726,1518,1551,251,4298,4090,4123,2823,13237, 13238,6014, 6013,13239, 13240,5770, 5769,13241 & 
& , 13242,6020, 6019,13243, 13244,5778, 5777], & 
& edgecnc=[4047,435,4048,313,4049,438,4050,317], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(582),elname="xbrick",eltype="xbrick",typekey=582) 

        call prepare(lib_xbrick(583),key=583, & 
& nodecnc=[248,1525,1554,1690,2820,4097,4126,4262,13245, 13246,13247, 13248,5734, 5733,9254, 9253,13249 & 
& , 13250,13251, 13252,5742, 5741,9260, 9259], & 
& edgecnc=[4051,4052,295,2055,4053,4054,299,2058], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(583),elname="xbrick",eltype="xbrick",typekey=583) 

        call prepare(lib_xbrick(584),key=584, & 
& nodecnc=[247,1555,1516,1722,2819,4127,4088,4294,13253, 13254,5718, 5717,13255, 13256,8970, 8969,13257 & 
& , 13258,5726, 5725,13259, 13260,8974, 8973], & 
& edgecnc=[4055,287,4056,1913,4057,291,4058,1915], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(584),elname="xbrick",eltype="xbrick",typekey=584) 

        call prepare(lib_xbrick(585),key=585, & 
& nodecnc=[1715,1526,1555,247,4287,4098,4127,2819,6336, 6335,13261, 13262,13254, 13253,5708, 5707,6342 & 
& , 6341,13263, 13264,13258, 13257,5716, 5715], & 
& edgecnc=[596,4059,4055,282,599,4060,4057,286], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(585),elname="xbrick",eltype="xbrick",typekey=585) 

        call prepare(lib_xbrick(586),key=586, & 
& nodecnc=[393,34,1559,1544,2965,2606,4131,4116,13265, 13266,13267, 13268,13269, 13270,10440, 10439,13271 & 
& , 13272,13273, 13274,13275, 13276,10446, 10445], & 
& edgecnc=[4061,4062,4063,2648,4064,4065,4066,2651], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(586),elname="xbrick",eltype="xbrick",typekey=586) 

        call prepare(lib_xbrick(587),key=587, & 
& nodecnc=[244,1219,1528,1544,2816,3791,4100,4116,13277, 13278,13279, 13280,10442, 10441,13281, 13282 & 
& ,13283, 13284,13285, 13286,10448, 10447,13287, 13288], & 
& edgecnc=[4067,4068,2649,4069,4070,4071,2652,4072], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(587),elname="xbrick",eltype="xbrick",typekey=587) 

        call prepare(lib_xbrick(588),key=588, & 
& nodecnc=[1219,243,1188,1528,3791,2815,3760,4100,10422, 10421,13289, 13290,6366, 6365,13280, 13279,10430 & 
& , 10429,13291, 13292,6374, 6373,13286, 13285], & 
& edgecnc=[2639,4073,611,4068,2643,4074,615,4071], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(588),elname="xbrick",eltype="xbrick",typekey=588) 

        call prepare(lib_xbrick(589),key=589, & 
& nodecnc=[396,1501,1464,36,2968,4073,4036,2608,13293, 13294,6520, 6519,6594, 6593,13295, 13296,13297 & 
& , 13298,6528, 6527,6602, 6601,13299, 13300], & 
& edgecnc=[4075,688,725,4076,4077,692,729,4078], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(589),elname="xbrick",eltype="xbrick",typekey=589) 

        call prepare(lib_xbrick(590),key=590, & 
& nodecnc=[1466,1482,1645,240,4038,4054,4217,2812,13301, 13302,13303, 13304,13305, 13306,13307, 13308 & 
& ,13309, 13310,13311, 13312,13313, 13314,13315, 13316], & 
& edgecnc=[4079,4080,4081,4082,4083,4084,4085,4086], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(590),elname="xbrick",eltype="xbrick",typekey=590) 

        call prepare(lib_xbrick(591),key=591, & 
& nodecnc=[239,1186,1472,1204,2811,3758,4044,3776,8412, 8411,13317, 13318,8382, 8381,13319, 13320,8420 & 
& , 8419,13321, 13322,8388, 8387,13323, 13324], & 
& edgecnc=[1634,4087,1619,4088,1638,4089,1622,4090], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(591),elname="xbrick",eltype="xbrick",typekey=591) 

        call prepare(lib_xbrick(592),key=592, & 
& nodecnc=[1229,1203,1171,1202,3801,3775,3743,3774,8394, 8393,8408, 8407,13325, 13326,13327, 13328,8402 & 
& , 8401,8416, 8415,13329, 13330,13331, 13332], & 
& edgecnc=[1625,1632,4091,4092,1629,1636,4093,4094], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(592),elname="xbrick",eltype="xbrick",typekey=592) 

        call prepare(lib_xbrick(593),key=593, & 
& nodecnc=[2207,1229,1202,1216,4779,3801,3774,3788,13333, 13334,13328, 13327,13335, 13336,13337, 13338 & 
& ,13339, 13340,13332, 13331,13341, 13342,13343, 13344], & 
& edgecnc=[4095,4092,4096,4097,4098,4094,4099,4100], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(593),elname="xbrick",eltype="xbrick",typekey=593) 

        call prepare(lib_xbrick(594),key=594, & 
& nodecnc=[1366,236,1398,1408,3938,2808,3970,3980,13345, 13346,11612, 11611,13347, 13348,11026, 11025 & 
& ,13349, 13350,11620, 11619,13351, 13352,11034, 11033], & 
& edgecnc=[4101,3234,4102,2941,4103,3238,4104,2945], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(594),elname="xbrick",eltype="xbrick",typekey=594) 

        call prepare(lib_xbrick(595),key=595, & 
& nodecnc=[1441,1651,1408,1398,4013,4223,3980,3970,13353, 13354,13355, 13356,13348, 13347,13357, 13358 & 
& ,13359, 13360,13361, 13362,13352, 13351,13363, 13364], & 
& edgecnc=[4105,4106,4102,4107,4108,4109,4104,4110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(595),elname="xbrick",eltype="xbrick",typekey=595) 

        call prepare(lib_xbrick(596),key=596, & 
& nodecnc=[1435,1434,1427,1438,4007,4006,3999,4010,12300, 12299,13365, 13366,13367, 13368,13369, 13370 & 
& ,12304, 12303,13371, 13372,13373, 13374,13375, 13376], & 
& edgecnc=[3578,4111,4112,4113,3580,4114,4115,4116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(596),elname="xbrick",eltype="xbrick",typekey=596) 

        call prepare(lib_xbrick(597),key=597, & 
& nodecnc=[407,41,1427,1416,2979,2613,3999,3988,13377, 13378,13379, 13380,13381, 13382,11624, 11623,13383 & 
& , 13384,13385, 13386,13387, 13388,11632, 11631], & 
& edgecnc=[4117,4118,4119,3240,4120,4121,4122,3244], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(597),elname="xbrick",eltype="xbrick",typekey=597) 

        call prepare(lib_xbrick(598),key=598, & 
& nodecnc=[234,1667,1477,1483,2806,4239,4049,4055,13389, 13390,13391, 13392,12230, 12229,12220, 12219 & 
& ,13393, 13394,13395, 13396,12238, 12237,12226, 12225], & 
& edgecnc=[4123,4124,3543,3538,4125,4126,3547,3541], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(598),elname="xbrick",eltype="xbrick",typekey=598) 

        call prepare(lib_xbrick(599),key=599, & 
& nodecnc=[410,409,1477,1484,2982,2981,4049,4056,13397, 13398,12232, 12231,13399, 13400,13401, 13402,13403 & 
& , 13404,12240, 12239,13405, 13406,13407, 13408], & 
& edgecnc=[4127,3544,4128,4129,4130,3548,4131,4132], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(599),elname="xbrick",eltype="xbrick",typekey=599) 

        call prepare(lib_xbrick(600),key=600, & 
& nodecnc=[411,43,1485,1470,2983,2615,4057,4042,13409, 13410,13411, 13412,13413, 13414,7400, 7399,13415 & 
& , 13416,13417, 13418,13419, 13420,7406, 7405], & 
& edgecnc=[4133,4134,4135,1128,4136,4137,4138,1131], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(600),elname="xbrick",eltype="xbrick",typekey=600) 

        call prepare(lib_xbrick(601),key=601, & 
& nodecnc=[412,44,1486,1469,2984,2616,4058,4041,13421, 13422,7402, 7401,13423, 13424,7372, 7371,13425 & 
& , 13426,7408, 7407,13427, 13428,7378, 7377], & 
& edgecnc=[4139,1129,4140,1114,4141,1132,4142,1117], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(601),elname="xbrick",eltype="xbrick",typekey=601) 

        call prepare(lib_xbrick(602),key=602, & 
& nodecnc=[230,1487,1468,1728,2802,4059,4040,4300,13429, 13430,7318, 7317,13431, 13432,7246, 7245,13433 & 
& , 13434,7326, 7325,13435, 13436,7254, 7253], & 
& edgecnc=[4143,1087,4144,1051,4145,1091,4146,1055], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(602),elname="xbrick",eltype="xbrick",typekey=602) 

        call prepare(lib_xbrick(603),key=603, & 
& nodecnc=[414,45,1487,1467,2986,2617,4059,4039,13437, 13438,7320, 7319,13439, 13440,7264, 7263,13441 & 
& , 13442,7328, 7327,13443, 13444,7272, 7271], & 
& edgecnc=[4147,1088,4148,1060,4149,1092,4150,1064], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(603),elname="xbrick",eltype="xbrick",typekey=603) 

        call prepare(lib_xbrick(604),key=604, & 
& nodecnc=[46,415,1488,1489,2618,2987,4060,4061,13445, 13446,7260, 7259,13447, 13448,6836, 6835,13449 & 
& , 13450,7268, 7267,13451, 13452,6842, 6841], & 
& edgecnc=[4151,1058,4152,846,4153,1062,4154,849], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(604),elname="xbrick",eltype="xbrick",typekey=604) 

        call prepare(lib_xbrick(605),key=605, & 
& nodecnc=[417,416,1586,1597,2989,2988,4158,4169,13453, 13454,6838, 6837,13455, 13456,13457, 13458,13459 & 
& , 13460,6844, 6843,13461, 13462,13463, 13464], & 
& edgecnc=[4155,847,4156,4157,4158,850,4159,4160], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(605),elname="xbrick",eltype="xbrick",typekey=605) 

        call prepare(lib_xbrick(606),key=606, & 
& nodecnc=[2104,227,1587,1598,4676,2799,4159,4170,13465, 13466,13467, 13468,13469, 13470,6848, 6847,13471 & 
& , 13472,13473, 13474,13475, 13476,6856, 6855], & 
& edgecnc=[4161,4162,4163,852,4164,4165,4166,856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(606),elname="xbrick",eltype="xbrick",typekey=606) 

        call prepare(lib_xbrick(607),key=607, & 
& nodecnc=[1599,1694,226,1589,4171,4266,2798,4161,13477, 13478,13479, 13480,13481, 13482,13483, 13484 & 
& ,13485, 13486,13487, 13488,13489, 13490,13491, 13492], & 
& edgecnc=[4167,4168,4169,4170,4171,4172,4173,4174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(607),elname="xbrick",eltype="xbrick",typekey=607) 

        call prepare(lib_xbrick(608),key=608, & 
& nodecnc=[421,1601,1590,49,2993,4173,4162,2621,7650, 7649,13493, 13494,7562, 7561,13495, 13496,7654, 7653 & 
& ,13497, 13498,7570, 7569,13499, 13500], & 
& edgecnc=[1253,4175,1209,4176,1255,4177,1213,4178], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(608),elname="xbrick",eltype="xbrick",typekey=608) 

        call prepare(lib_xbrick(609),key=609, & 
& nodecnc=[422,1602,1560,423,2994,4174,4132,2995,7566, 7565,13501, 13502,7580, 7579,13503, 13504,7574 & 
& , 7573,13505, 13506,7588, 7587,13507, 13508], & 
& edgecnc=[1211,4179,1218,4180,1215,4181,1222,4182], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(609),elname="xbrick",eltype="xbrick",typekey=609) 

        call prepare(lib_xbrick(610),key=610, & 
& nodecnc=[424,1560,1508,425,2996,4132,4080,2997,7582, 7581,13509, 13510,13511, 13512,13513, 13514,7590 & 
& , 7589,13515, 13516,13517, 13518,13519, 13520], & 
& edgecnc=[1219,4183,4184,4185,1223,4186,4187,4188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(610),elname="xbrick",eltype="xbrick",typekey=610) 

        call prepare(lib_xbrick(611),key=611, & 
& nodecnc=[425,1508,1505,52,2997,4080,4077,2624,13512, 13511,7600, 7599,13521, 13522,13523, 13524,13518 & 
& , 13517,7608, 7607,13525, 13526,13527, 13528], & 
& edgecnc=[4184,1228,4189,4190,4187,1232,4191,4192], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(611),elname="xbrick",eltype="xbrick",typekey=611) 

        call prepare(lib_xbrick(612),key=612, & 
& nodecnc=[258,1677,1506,1509,2830,4249,4078,4081,7614, 7613,13529, 13530,7664, 7663,7676, 7675,7622, 7621 & 
& ,13531, 13532,7672, 7671,7682, 7681], & 
& edgecnc=[1235,4193,1260,1266,1239,4194,1264,1269], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(612),elname="xbrick",eltype="xbrick",typekey=612) 

        call prepare(lib_xbrick(613),key=613, & 
& nodecnc=[53,1506,1510,427,2625,4078,4082,2999,7658, 7657,13533, 13534,7714, 7713,13535, 13536,7666, 7665 & 
& ,13537, 13538,7722, 7721,13539, 13540], & 
& edgecnc=[1257,4195,1285,4196,1261,4197,1289,4198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(613),elname="xbrick",eltype="xbrick",typekey=613) 

        call prepare(lib_xbrick(614),key=614, & 
& nodecnc=[54,1530,1561,429,2626,4102,4133,3001,7740, 7739,7772, 7771,13541, 13542,13543, 13544,7746, 7745 & 
& ,7780, 7779,13545, 13546,13547, 13548], & 
& edgecnc=[1298,1314,4199,4200,1301,1318,4201,4202], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(614),elname="xbrick",eltype="xbrick",typekey=614) 

        call prepare(lib_xbrick(615),key=615, & 
& nodecnc=[430,1562,1531,55,3002,4134,4103,2627,13549, 13550,13551, 13552,7754, 7753,13553, 13554,13555 & 
& , 13556,13557, 13558,7762, 7761,13559, 13560], & 
& edgecnc=[4203,4204,1305,4205,4206,4207,1309,4208], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(615),elname="xbrick",eltype="xbrick",typekey=615) 

        call prepare(lib_xbrick(616),key=616, & 
& nodecnc=[56,432,1564,1532,2628,3004,4136,4104,13561, 13562,13563, 13564,7802, 7801,13565, 13566,13567 & 
& , 13568,13569, 13570,7808, 7807,13571, 13572], & 
& edgecnc=[4209,4210,1329,4211,4212,4213,1332,4214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(616),elname="xbrick",eltype="xbrick",typekey=616) 

        call prepare(lib_xbrick(617),key=617, & 
& nodecnc=[1566,1533,1684,325,4138,4105,4256,2897,13573, 13574,7812, 7811,13575, 13576,13577, 13578,13579 & 
& , 13580,7820, 7819,13581, 13582,13583, 13584], & 
& edgecnc=[4215,1334,4216,4217,4218,1338,4219,4220], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(617),elname="xbrick",eltype="xbrick",typekey=617) 

        call prepare(lib_xbrick(618),key=618, & 
& nodecnc=[57,1533,1566,434,2629,4105,4138,3006,13585, 13586,13574, 13573,13587, 13588,13589, 13590,13591 & 
& , 13592,13580, 13579,13593, 13594,13595, 13596], & 
& edgecnc=[4221,4215,4222,4223,4224,4218,4225,4226], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(618),elname="xbrick",eltype="xbrick",typekey=618) 

        call prepare(lib_xbrick(619),key=619, & 
& nodecnc=[435,2205,436,58,3007,4777,3008,2630,13597, 13598,7540, 7539,13599, 13600,13601, 13602,13603 & 
& , 13604,7548, 7547,13605, 13606,13607, 13608], & 
& edgecnc=[4227,1198,4228,4229,4230,1202,4231,4232], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(619),elname="xbrick",eltype="xbrick",typekey=619) 

        call prepare(lib_xbrick(620),key=620, & 
& nodecnc=[325,1603,2205,1566,2897,4175,4777,4138,13609, 13610,7534, 7533,13611, 13612,13578, 13577,13613 & 
& , 13614,7542, 7541,13615, 13616,13584, 13583], & 
& edgecnc=[4233,1195,4234,4217,4235,1199,4236,4220], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(620),elname="xbrick",eltype="xbrick",typekey=620) 

        call prepare(lib_xbrick(621),key=621, & 
& nodecnc=[1603,1490,59,437,4175,4062,2631,3009,13617, 13618,13619, 13620,13621, 13622,7536, 7535,13623 & 
& , 13624,13625, 13626,13627, 13628,7544, 7543], & 
& edgecnc=[4237,4238,4239,1196,4240,4241,4242,1200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(621),elname="xbrick",eltype="xbrick",typekey=621) 

        call prepare(lib_xbrick(622),key=622, & 
& nodecnc=[60,439,1491,1492,2632,3011,4063,4064,13629, 13630,6964, 6963,6976, 6975,6992, 6991,13631, 13632 & 
& ,6972, 6971,6984, 6983,7000, 6999], & 
& edgecnc=[4243,910,916,924,4244,914,920,928], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(622),elname="xbrick",eltype="xbrick",typekey=622) 

        call prepare(lib_xbrick(623),key=623, & 
& nodecnc=[441,440,1529,1567,3013,3012,4101,4139,13633, 13634,6996, 6995,7010, 7009,13635, 13636,13637 & 
& , 13638,7004, 7003,7018, 7017,13639, 13640], & 
& edgecnc=[4245,926,933,4246,4247,930,937,4248], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(623),elname="xbrick",eltype="xbrick",typekey=623) 

        call prepare(lib_xbrick(624),key=624, & 
& nodecnc=[442,61,1568,1535,3014,2633,4140,4107,13641, 13642,13643, 13644,13645, 13646,8076, 8075,13647 & 
& , 13648,13649, 13650,13651, 13652,8084, 8083], & 
& edgecnc=[4249,4250,4251,1466,4252,4253,4254,1470], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(624),elname="xbrick",eltype="xbrick",typekey=624) 

        call prepare(lib_xbrick(625),key=625, & 
& nodecnc=[444,62,1570,1546,3016,2634,4142,4118,13653, 13654,13655, 13656,13657, 13658,8044, 8043,13659 & 
& , 13660,13661, 13662,13663, 13664,8052, 8051], & 
& edgecnc=[4255,4256,4257,1450,4258,4259,4260,1454], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(625),elname="xbrick",eltype="xbrick",typekey=625) 

        call prepare(lib_xbrick(626),key=626, & 
& nodecnc=[330,1773,1534,1546,2902,4345,4106,4118,10638, 10637,13665, 13666,8038, 8037,13667, 13668,10646 & 
& , 10645,13669, 13670,8046, 8045,13671, 13672], & 
& edgecnc=[2747,4261,1447,4262,2751,4263,1451,4264], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(626),elname="xbrick",eltype="xbrick",typekey=626) 

        call prepare(lib_xbrick(627),key=627, & 
& nodecnc=[1727,1536,1571,331,4299,4108,4143,2903,8106, 8105,8086, 8085,13673, 13674,8026, 8025,8114, 8113 & 
& ,8094, 8093,13675, 13676,8034, 8033], & 
& edgecnc=[1481,1471,4265,1441,1485,1475,4266,1445], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(627),elname="xbrick",eltype="xbrick",typekey=627) 

        call prepare(lib_xbrick(628),key=628, & 
& nodecnc=[447,446,1536,1572,3019,3018,4108,4144,13677, 13678,8088, 8087,8104, 8103,13679, 13680,13681 & 
& , 13682,8096, 8095,8112, 8111,13683, 13684], & 
& edgecnc=[4267,1472,1480,4268,4269,1476,1484,4270], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(628),elname="xbrick",eltype="xbrick",typekey=628) 

        call prepare(lib_xbrick(629),key=629, & 
& nodecnc=[448,64,1573,1537,3020,2636,4145,4109,13685, 13686,13687, 13688,8120, 8119,13689, 13690,13691 & 
& , 13692,13693, 13694,8128, 8127,13695, 13696], & 
& edgecnc=[4271,4272,1488,4273,4274,4275,1492,4276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(629),elname="xbrick",eltype="xbrick",typekey=629) 

        call prepare(lib_xbrick(630),key=630, & 
& nodecnc=[335,1575,1540,1723,2907,4147,4112,4295,13697, 13698,13699, 13700,13701, 13702,8196, 8195,13703 & 
& , 13704,13705, 13706,13707, 13708,8202, 8201], & 
& edgecnc=[4277,4278,4279,1526,4280,4281,4282,1529], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(630),elname="xbrick",eltype="xbrick",typekey=630) 

        call prepare(lib_xbrick(631),key=631, & 
& nodecnc=[451,66,1575,1539,3023,2638,4147,4111,13709, 13710,13711, 13712,13713, 13714,8154, 8153,13715 & 
& , 13716,13717, 13718,13719, 13720,8160, 8159], & 
& edgecnc=[4283,4284,4285,1505,4286,4287,4288,1508], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(631),elname="xbrick",eltype="xbrick",typekey=631) 

        call prepare(lib_xbrick(632),key=632, & 
& nodecnc=[453,67,1577,1541,3025,2639,4149,4113,13721, 13722,13723, 13724,13725, 13726,8220, 8219,13727 & 
& , 13728,13729, 13730,13731, 13732,8228, 8227], & 
& edgecnc=[4289,4290,4291,1538,4292,4293,4294,1542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(632),elname="xbrick",eltype="xbrick",typekey=632) 

        call prepare(lib_xbrick(633),key=633, & 
& nodecnc=[68,454,1578,1429,2640,3026,4150,4001,13733, 13734,8224, 8223,13735, 13736,13737, 13738,13739 & 
& , 13740,8232, 8231,13741, 13742,13743, 13744], & 
& edgecnc=[4295,1540,4296,4297,4298,1544,4299,4300], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(633),elname="xbrick",eltype="xbrick",typekey=633) 

        call prepare(lib_xbrick(634),key=634, & 
& nodecnc=[455,68,1429,1636,3027,2640,4001,4208,13745, 13746,13738, 13737,8554, 8553,13747, 13748,13749 & 
& , 13750,13744, 13743,8562, 8561,13751, 13752], & 
& edgecnc=[4301,4297,1705,4302,4303,4300,1709,4304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(634),elname="xbrick",eltype="xbrick",typekey=634) 

        call prepare(lib_xbrick(635),key=635, & 
& nodecnc=[1802,1705,1473,1493,4374,4277,4045,4065,13753, 13754,8578, 8577,13755, 13756,13757, 13758,13759 & 
& , 13760,8586, 8585,13761, 13762,13763, 13764], & 
& edgecnc=[4305,1717,4306,4307,4308,1721,4309,4310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(635),elname="xbrick",eltype="xbrick",typekey=635) 

        call prepare(lib_xbrick(636),key=636, & 
& nodecnc=[340,1152,1473,1154,2912,3724,4045,3726,8674, 8673,13765, 13766,8576, 8575,13767, 13768,8680 & 
& , 8679,13769, 13770,8584, 8583,13771, 13772], & 
& edgecnc=[1765,4311,1716,4312,1768,4313,1720,4314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(636),elname="xbrick",eltype="xbrick",typekey=636) 

        call prepare(lib_xbrick(637),key=637, & 
& nodecnc=[1068,1072,1111,340,3640,3644,3683,2912,13773, 13774,13775, 13776,8670, 8669,13777, 13778,13779 & 
& , 13780,13781, 13782,8676, 8675,13783, 13784], & 
& edgecnc=[4315,4316,1763,4317,4318,4319,1766,4320], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(637),elname="xbrick",eltype="xbrick",typekey=637) 

        call prepare(lib_xbrick(638),key=638, & 
& nodecnc=[1323,1309,1299,1313,3895,3881,3871,3885,13785, 13786,13787, 13788,11262, 11261,13789, 13790 & 
& ,13791, 13792,13793, 13794,11270, 11269,13795, 13796], & 
& edgecnc=[4321,4322,3059,4323,4324,4325,3063,4326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(638),elname="xbrick",eltype="xbrick",typekey=638) 

        call prepare(lib_xbrick(639),key=639, & 
& nodecnc=[1282,1281,1313,1287,3854,3853,3885,3859,13797, 13798,11278, 11277,11268, 11267,13799, 13800 & 
& ,13801, 13802,11286, 11285,11276, 11275,13803, 13804], & 
& edgecnc=[4327,3067,3062,4328,4329,3071,3066,4330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(639),elname="xbrick",eltype="xbrick",typekey=639) 

        call prepare(lib_xbrick(640),key=640, & 
& nodecnc=[464,1281,1262,73,3036,3853,3834,2645,11280, 11279,13805, 13806,8786, 8785,13807, 13808,11288 & 
& , 11287,13809, 13810,8794, 8793,13811, 13812], & 
& edgecnc=[3068,4331,1821,4332,3072,4333,1825,4334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(640),elname="xbrick",eltype="xbrick",typekey=640) 

        call prepare(lib_xbrick(641),key=641, & 
& nodecnc=[1235,345,1259,1223,3807,2917,3831,3795,9726, 9725,13813, 13814,8806, 8805,13815, 13816,9734 & 
& , 9733,13817, 13818,8814, 8813,13819, 13820], & 
& edgecnc=[2291,4335,1831,4336,2295,4337,1835,4338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(641),elname="xbrick",eltype="xbrick",typekey=641) 

        call prepare(lib_xbrick(642),key=642, & 
& nodecnc=[1257,1502,1258,345,3829,4074,3830,2917,5590, 5589,13821, 13822,13823, 13824,9732, 9731,5596 & 
& , 5595,13825, 13826,13827, 13828,9740, 9739], & 
& edgecnc=[223,4339,4340,2294,226,4341,4342,2298], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(642),elname="xbrick",eltype="xbrick",typekey=642) 

        call prepare(lib_xbrick(643),key=643, & 
& nodecnc=[1495,468,467,1494,4067,3040,3039,4066,13829, 13830,13831, 13832,13833, 13834,5570, 5569,13835 & 
& , 13836,13837, 13838,13839, 13840,5578, 5577], & 
& edgecnc=[4343,4344,4345,213,4346,4347,4348,217], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(643),elname="xbrick",eltype="xbrick",typekey=643) 

        call prepare(lib_xbrick(644),key=644, & 
& nodecnc=[468,1495,1462,75,3040,4067,4034,2647,13830, 13829,13841, 13842,5602, 5601,13843, 13844,13836 & 
& , 13835,13845, 13846,5610, 5609,13847, 13848], & 
& edgecnc=[4343,4349,229,4350,4346,4351,233,4352], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(644),elname="xbrick",eltype="xbrick",typekey=644) 

        call prepare(lib_xbrick(645),key=645, & 
& nodecnc=[1421,348,1369,1385,3993,2920,3941,3957,5626, 5625,13849, 13850,13851, 13852,13853, 13854,5634 & 
& , 5633,13855, 13856,13857, 13858,13859, 13860], & 
& edgecnc=[241,4353,4354,4355,245,4356,4357,4358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(645),elname="xbrick",eltype="xbrick",typekey=645) 

        call prepare(lib_xbrick(646),key=646, & 
& nodecnc=[470,1496,2208,76,3042,4068,4780,2648,5616, 5615,13861, 13862,13863, 13864,13865, 13866,5622 & 
& , 5621,13867, 13868,13869, 13870,13871, 13872], & 
& edgecnc=[236,4359,4360,4361,239,4362,4363,4364], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(646),elname="xbrick",eltype="xbrick",typekey=646) 

        call prepare(lib_xbrick(647),key=647, & 
& nodecnc=[77,472,1640,1497,2649,3044,4212,4069,13873, 13874,13875, 13876,13877, 13878,13879, 13880,13881 & 
& , 13882,13883, 13884,13885, 13886,13887, 13888], & 
& edgecnc=[4365,4366,4367,4368,4369,4370,4371,4372], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(647),elname="xbrick",eltype="xbrick",typekey=647) 

        call prepare(lib_xbrick(648),key=648, & 
& nodecnc=[473,77,1497,1478,3045,2649,4069,4050,13889, 13890,13880, 13879,13891, 13892,13024, 13023,13893 & 
& , 13894,13888, 13887,13895, 13896,13032, 13031], & 
& edgecnc=[4373,4368,4374,3940,4375,4372,4376,3944], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(648),elname="xbrick",eltype="xbrick",typekey=648) 

        call prepare(lib_xbrick(649),key=649, & 
& nodecnc=[474,78,1498,1475,3046,2650,4070,4047,13897, 13898,13020, 13019,13899, 13900,13901, 13902,13903 & 
& , 13904,13028, 13027,13905, 13906,13907, 13908], & 
& edgecnc=[4377,3938,4378,4379,4380,3942,4381,4382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(649),elname="xbrick",eltype="xbrick",typekey=649) 

        call prepare(lib_xbrick(650),key=650, & 
& nodecnc=[1457,1458,1460,351,4029,4030,4032,2923,13909, 13910,13911, 13912,9534, 9533,13913, 13914,13915 & 
& , 13916,13917, 13918,9542, 9541,13919, 13920], & 
& edgecnc=[4383,4384,2195,4385,4386,4387,2199,4388], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(650),elname="xbrick",eltype="xbrick",typekey=650) 

        call prepare(lib_xbrick(651),key=651, & 
& nodecnc=[1719,1474,1503,352,4291,4046,4075,2924,13060, 13059,13921, 13922,13923, 13924,13925, 13926 & 
& ,13066, 13065,13927, 13928,13929, 13930,13931, 13932], & 
& edgecnc=[3958,4389,4390,4391,3961,4392,4393,4394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(651),elname="xbrick",eltype="xbrick",typekey=651) 

        call prepare(lib_xbrick(652),key=652, & 
& nodecnc=[80,477,1499,1500,2652,3049,4071,4072,13933, 13934,9436, 9435,13935, 13936,9408, 9407,13937 & 
& , 13938,9444, 9443,13939, 13940,9416, 9415], & 
& edgecnc=[4395,2146,4396,2132,4397,2150,4398,2136], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(652),elname="xbrick",eltype="xbrick",typekey=652) 

        call prepare(lib_xbrick(653),key=653, & 
& nodecnc=[479,478,1594,1604,3051,3050,4166,4176,13941, 13942,9412, 9411,13943, 13944,13945, 13946,13947 & 
& , 13948,9420, 9419,13949, 13950,13951, 13952], & 
& edgecnc=[4399,2134,4400,4401,4402,2138,4403,4404], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(653),elname="xbrick",eltype="xbrick",typekey=653) 

        call prepare(lib_xbrick(654),key=654, & 
& nodecnc=[480,81,1605,1593,3052,2653,4177,4165,13953, 13954,13955, 13956,13957, 13958,9156, 9155,13959 & 
& , 13960,13961, 13962,13963, 13964,9164, 9163], & 
& edgecnc=[4405,4406,4407,2006,4408,4409,4410,2010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(654),elname="xbrick",eltype="xbrick",typekey=654) 

        call prepare(lib_xbrick(655),key=655, & 
& nodecnc=[481,1606,1592,82,3053,4178,4164,2654,9160, 9159,13965, 13966,9136, 9135,13967, 13968,9168, 9167 & 
& ,13969, 13970,9144, 9143,13971, 13972], & 
& edgecnc=[2008,4411,1996,4412,2012,4413,2000,4414], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(655),elname="xbrick",eltype="xbrick",typekey=655) 

        call prepare(lib_xbrick(656),key=656, & 
& nodecnc=[483,1608,1591,83,3055,4180,4163,2655,9146, 9145,13973, 13974,9050, 9049,13975, 13976,9150, 9149 & 
& ,13977, 13978,9058, 9057,13979, 13980], & 
& edgecnc=[2001,4415,1953,4416,2003,4417,1957,4418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(656),elname="xbrick",eltype="xbrick",typekey=656) 

        call prepare(lib_xbrick(657),key=657, & 
& nodecnc=[484,1609,1580,485,3056,4181,4152,3057,9054, 9053,13981, 13982,5880, 5879,13983, 13984,9062 & 
& , 9061,13985, 13986,5888, 5887,13987, 13988], & 
& edgecnc=[1955,4419,368,4420,1959,4421,372,4422], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(657),elname="xbrick",eltype="xbrick",typekey=657) 

        call prepare(lib_xbrick(658),key=658, & 
& nodecnc=[486,1580,1512,487,3058,4152,4084,3059,5882, 5881,13989, 13990,13991, 13992,13993, 13994,5890 & 
& , 5889,13995, 13996,13997, 13998,13999, 14000], & 
& edgecnc=[369,4423,4424,4425,373,4426,4427,4428], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(658),elname="xbrick",eltype="xbrick",typekey=658) 

        call prepare(lib_xbrick(659),key=659, & 
& nodecnc=[487,1512,1504,85,3059,4084,4076,2657,13992, 13991,14001, 14002,5898, 5897,14003, 14004,13998 & 
& , 13997,14005, 14006,5906, 5905,14007, 14008], & 
& edgecnc=[4424,4429,377,4430,4427,4431,381,4432], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(659),elname="xbrick",eltype="xbrick",typekey=659) 

        call prepare(lib_xbrick(660),key=660, & 
& nodecnc=[324,1678,1507,1513,2896,4250,4079,4085,11446, 11445,14009, 14010,5914, 5913,5926, 5925,11454 & 
& , 11453,14011, 14012,5920, 5919,5932, 5931], & 
& edgecnc=[3151,4433,385,391,3155,4434,388,394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(660),elname="xbrick",eltype="xbrick",typekey=660) 

        call prepare(lib_xbrick(661),key=661, & 
& nodecnc=[86,1507,1514,489,2658,4079,4086,3061,5910, 5909,14013, 14014,9566, 9565,14015, 14016,5916, 5915 & 
& ,14017, 14018,9574, 9573,14019, 14020], & 
& edgecnc=[383,4435,2211,4436,386,4437,2215,4438], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(661),elname="xbrick",eltype="xbrick",typekey=661) 

        call prepare(lib_xbrick(662),key=662, & 
& nodecnc=[87,1542,1581,491,2659,4114,4153,3063,9624, 9623,9640, 9639,14021, 14022,14023, 14024,9630, 9629 & 
& ,9648, 9647,14025, 14026,14027, 14028], & 
& edgecnc=[2240,2248,4439,4440,2243,2252,4441,4442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(662),elname="xbrick",eltype="xbrick",typekey=662) 

        call prepare(lib_xbrick(663),key=663, & 
& nodecnc=[492,1582,1543,88,3064,4154,4115,2660,14029, 14030,14031, 14032,9678, 9677,14033, 14034,14035 & 
& , 14036,14037, 14038,9686, 9685,14039, 14040], & 
& edgecnc=[4443,4444,2267,4445,4446,4447,2271,4448], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(663),elname="xbrick",eltype="xbrick",typekey=663) 

        call prepare(lib_xbrick(664),key=664, & 
& nodecnc=[89,494,1584,1520,2661,3066,4156,4092,14041, 14042,14043, 14044,14045, 14046,14047, 14048,14049 & 
& , 14050,14051, 14052,14053, 14054,14055, 14056], & 
& edgecnc=[4449,4450,4451,4452,4453,4454,4455,4456], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(664),elname="xbrick",eltype="xbrick",typekey=664) 

        call prepare(lib_xbrick(665),key=665, & 
& nodecnc=[1519,1545,1685,257,4091,4117,4257,2829,12450, 12449,14057, 14058,14059, 14060,13168, 13167 & 
& ,12456, 12455,14061, 14062,14063, 14064,13176, 13175], & 
& edgecnc=[3653,4457,4458,4012,3656,4459,4460,4016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(665),elname="xbrick",eltype="xbrick",typekey=665) 

        call prepare(lib_xbrick(666),key=666, & 
& nodecnc=[2206,1596,375,374,4778,4168,2947,2946,13164, 13163,13184, 13183,14065, 14066,6032, 6031,13172 & 
& , 13171,13192, 13191,14067, 14068,6040, 6039], & 
& edgecnc=[4010,4020,4461,444,4014,4024,4462,448], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(666),elname="xbrick",eltype="xbrick",typekey=666) 

        call prepare(lib_xbrick(667),key=667, & 
& nodecnc=[526,358,24,498,3098,2930,2596,3070,5150, 5149,14069, 14070,13154, 13153,14071, 14072,5158, 5157 & 
& ,14073, 14074,13158, 13157,14075, 14076], & 
& edgecnc=[3,4463,4005,4464,7,4465,4007,4466], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(667),elname="xbrick",eltype="xbrick",typekey=667) 

        call prepare(lib_xbrick(668),key=668, & 
& nodecnc=[581,2248,629,626,3153,4820,3201,3198,14077, 14078,14079, 14080,14081, 14082,14083, 14084,14085 & 
& , 14086,14087, 14088,14089, 14090,14091, 14092], & 
& edgecnc=[4467,4468,4469,4470,4471,4472,4473,4474], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(668),elname="xbrick",eltype="xbrick",typekey=668) 

        call prepare(lib_xbrick(669),key=669, & 
& nodecnc=[566,547,592,586,3138,3119,3164,3158,14093, 14094,14095, 14096,14097, 14098,14099, 14100,14101 & 
& , 14102,14103, 14104,14105, 14106,14107, 14108], & 
& edgecnc=[4475,4476,4477,4478,4479,4480,4481,4482], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(669),elname="xbrick",eltype="xbrick",typekey=669) 

        call prepare(lib_xbrick(670),key=670, & 
& nodecnc=[527,500,550,547,3099,3072,3122,3119,5466, 5465,14109, 14110,14111, 14112,14113, 14114,5472 & 
& , 5471,14115, 14116,14117, 14118,14119, 14120], & 
& edgecnc=[161,4483,4484,4485,164,4486,4487,4488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(670),elname="xbrick",eltype="xbrick",typekey=670) 

        call prepare(lib_xbrick(671),key=671, & 
& nodecnc=[596,560,518,553,3168,3132,3090,3125,14121, 14122,14123, 14124,14125, 14126,14127, 14128,14129 & 
& , 14130,14131, 14132,14133, 14134,14135, 14136], & 
& edgecnc=[4489,4490,4491,4492,4493,4494,4495,4496], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(671),elname="xbrick",eltype="xbrick",typekey=671) 

        call prepare(lib_xbrick(672),key=672, & 
& nodecnc=[529,5,4,501,3101,2577,2576,3073,5198, 5197,14137, 14138,13106, 13105,5212, 5211,5206, 5205 & 
& ,14139, 14140,13110, 13109,5218, 5217], & 
& edgecnc=[27,4497,3981,34,31,4498,3983,37], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(672),elname="xbrick",eltype="xbrick",typekey=672) 

        call prepare(lib_xbrick(673),key=673, & 
& nodecnc=[563,543,502,522,3135,3115,3074,3094,14141, 14142,6582, 6581,5480, 5479,14143, 14144,14145, 14146 & 
& ,6586, 6585,5488, 5487,14147, 14148], & 
& edgecnc=[4499,719,168,4500,4501,721,172,4502], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(673),elname="xbrick",eltype="xbrick",typekey=673) 

        call prepare(lib_xbrick(674),key=674, & 
& nodecnc=[523,516,541,558,3095,3088,3113,3130,14149, 14150,14151, 14152,14153, 14154,14155, 14156,14157 & 
& , 14158,14159, 14160,14161, 14162,14163, 14164], & 
& edgecnc=[4503,4504,4505,4506,4507,4508,4509,4510], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(674),elname="xbrick",eltype="xbrick",typekey=674) 

        call prepare(lib_xbrick(675),key=675, & 
& nodecnc=[533,363,8,503,3105,2935,2580,3075,5254, 5253,14165, 14166,5240, 5239,14167, 14168,5262, 5261 & 
& ,14169, 14170,5246, 5245,14171, 14172], & 
& edgecnc=[55,4511,48,4512,59,4513,51,4514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(675),elname="xbrick",eltype="xbrick",typekey=675) 

        call prepare(lib_xbrick(676),key=676, & 
& nodecnc=[574,551,504,530,3146,3123,3076,3102,14173, 14174,14175, 14176,5344, 5343,14177, 14178,14179 & 
& , 14180,14181, 14182,5352, 5351,14183, 14184], & 
& edgecnc=[4515,4516,100,4517,4518,4519,104,4520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(676),elname="xbrick",eltype="xbrick",typekey=676) 

        call prepare(lib_xbrick(677),key=677, & 
& nodecnc=[514,367,14,504,3086,2939,2586,3076,5362, 5361,14185, 14186,5346, 5345,14187, 14188,5370, 5369 & 
& ,14189, 14190,5354, 5353,14191, 14192], & 
& edgecnc=[109,4521,101,4522,113,4523,105,4524], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(677),elname="xbrick",eltype="xbrick",typekey=677) 

        call prepare(lib_xbrick(678),key=678, & 
& nodecnc=[505,531,564,545,3077,3103,3136,3117,5440, 5439,14193, 14194,14195, 14196,14197, 14198,5448 & 
& , 5447,14199, 14200,14201, 14202,14203, 14204], & 
& edgecnc=[148,4525,4526,4527,152,4528,4529,4530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(678),elname="xbrick",eltype="xbrick",typekey=678) 

        call prepare(lib_xbrick(679),key=679, & 
& nodecnc=[534,11,10,525,3106,2583,2582,3097,5302, 5301,14205, 14206,5292, 5291,14207, 14208,5310, 5309 & 
& ,14209, 14210,5298, 5297,14211, 14212], & 
& edgecnc=[79,4531,74,4532,83,4533,77,4534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(679),elname="xbrick",eltype="xbrick",typekey=679) 

        call prepare(lib_xbrick(680),key=680, & 
& nodecnc=[520,17,16,508,3092,2589,2588,3080,5398, 5397,14213, 14214,5388, 5387,6698, 6697,5406, 5405 & 
& ,14215, 14216,5394, 5393,6706, 6705], & 
& edgecnc=[127,4535,122,777,131,4536,125,781], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(680),elname="xbrick",eltype="xbrick",typekey=680) 

        call prepare(lib_xbrick(681),key=681, & 
& nodecnc=[507,532,567,552,3079,3104,3139,3124,5374, 5373,14217, 14218,14219, 14220,14221, 14222,5380 & 
& , 5379,14223, 14224,14225, 14226,14227, 14228], & 
& edgecnc=[115,4537,4538,4539,118,4540,4541,4542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(681),elname="xbrick",eltype="xbrick",typekey=681) 

        call prepare(lib_xbrick(682),key=682, & 
& nodecnc=[557,544,2241,588,3129,3116,4813,3160,14229, 14230,14231, 14232,14233, 14234,14235, 14236,14237 & 
& , 14238,14239, 14240,14241, 14242,14243, 14244], & 
& edgecnc=[4543,4544,4545,4546,4547,4548,4549,4550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(682),elname="xbrick",eltype="xbrick",typekey=682) 

        call prepare(lib_xbrick(683),key=683, & 
& nodecnc=[523,362,6,516,3095,2934,2578,3088,5226, 5225,14245, 14246,13114, 13113,14150, 14149,5234, 5233 & 
& ,14247, 14248,13120, 13119,14158, 14157], & 
& edgecnc=[41,4551,3985,4503,45,4552,3988,4507], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(683),elname="xbrick",eltype="xbrick",typekey=683) 

        call prepare(lib_xbrick(684),key=684, & 
& nodecnc=[516,509,568,541,3088,3081,3140,3113,13118, 13117,14249, 14250,14251, 14252,14152, 14151,13124 & 
& , 13123,14253, 14254,14255, 14256,14160, 14159], & 
& edgecnc=[3987,4553,4554,4504,3990,4555,4556,4508], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(684),elname="xbrick",eltype="xbrick",typekey=684) 

        call prepare(lib_xbrick(685),key=685, & 
& nodecnc=[2210,510,359,2,4782,3082,2931,2574,14257, 14258,14259, 14260,14261, 14262,14263, 14264,14265 & 
& , 14266,14267, 14268,14269, 14270,14271, 14272], & 
& edgecnc=[4557,4558,4559,4560,4561,4562,4563,4564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(685),elname="xbrick",eltype="xbrick",typekey=685) 

        call prepare(lib_xbrick(686),key=686, & 
& nodecnc=[510,519,3,359,3082,3091,2575,2931,14273, 14274,14275, 14276,14277, 14278,14260, 14259,14279 & 
& , 14280,14281, 14282,14283, 14284,14268, 14267], & 
& edgecnc=[4565,4566,4567,4558,4568,4569,4570,4562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(686),elname="xbrick",eltype="xbrick",typekey=686) 

        call prepare(lib_xbrick(687),key=687, & 
& nodecnc=[518,560,542,512,3090,3132,3114,3084,14124, 14123,14285, 14286,5178, 5177,8864, 8863,14132, 14131 & 
& ,14287, 14288,5186, 5185,8872, 8871], & 
& edgecnc=[4490,4571,17,1860,4494,4572,21,1864], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(687),elname="xbrick",eltype="xbrick",typekey=687) 

        call prepare(lib_xbrick(688),key=688, & 
& nodecnc=[556,572,535,517,3128,3144,3107,3089,14289, 14290,14291, 14292,5324, 5323,14293, 14294,14295 & 
& , 14296,14297, 14298,5332, 5331,14299, 14300], & 
& edgecnc=[4573,4574,90,4575,4576,4577,94,4578], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(688),elname="xbrick",eltype="xbrick",typekey=688) 

        call prepare(lib_xbrick(689),key=689, & 
& nodecnc=[551,546,514,504,3123,3118,3086,3076,14301, 14302,14303, 14304,14188, 14187,14176, 14175,14305 & 
& , 14306,14307, 14308,14192, 14191,14182, 14181], & 
& edgecnc=[4579,4580,4522,4516,4581,4582,4524,4519], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(689),elname="xbrick",eltype="xbrick",typekey=689) 

        call prepare(lib_xbrick(690),key=690, & 
& nodecnc=[549,544,515,520,3121,3116,3087,3092,14309, 14310,14311, 14312,5400, 5399,6704, 6703,14313, 14314 & 
& ,14315, 14316,5408, 5407,6712, 6711], & 
& edgecnc=[4583,4584,128,780,4585,4586,132,784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(690),elname="xbrick",eltype="xbrick",typekey=690) 

        call prepare(lib_xbrick(691),key=691, & 
& nodecnc=[3,519,553,518,2575,3091,3125,3090,14276, 14275,14317, 14318,14126, 14125,8862, 8861,14282, 14281 & 
& ,14319, 14320,14134, 14133,8870, 8869], & 
& edgecnc=[4566,4587,4491,1859,4569,4588,4495,1863], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(691),elname="xbrick",eltype="xbrick",typekey=691) 

        call prepare(lib_xbrick(692),key=692, & 
& nodecnc=[510,561,2216,519,3082,3133,4788,3091,14321, 14322,14323, 14324,14325, 14326,14274, 14273,14327 & 
& , 14328,14329, 14330,14331, 14332,14280, 14279], & 
& edgecnc=[4589,4590,4591,4565,4592,4593,4594,4568], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(692),elname="xbrick",eltype="xbrick",typekey=692) 

        call prepare(lib_xbrick(693),key=693, & 
& nodecnc=[531,19,370,524,3103,2591,2942,3096,5438, 5437,14333, 14334,5428, 5427,14335, 14336,5446, 5445 & 
& ,14337, 14338,5434, 5433,14339, 14340], & 
& edgecnc=[147,4595,142,4596,151,4597,145,4598], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(693),elname="xbrick",eltype="xbrick",typekey=693) 

        call prepare(lib_xbrick(694),key=694, & 
& nodecnc=[521,515,544,557,3093,3087,3116,3129,5414, 5413,14312, 14311,14230, 14229,14341, 14342,5420 & 
& , 5419,14316, 14315,14238, 14237,14343, 14344], & 
& edgecnc=[135,4584,4543,4599,138,4586,4547,4600], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(694),elname="xbrick",eltype="xbrick",typekey=694) 

        call prepare(lib_xbrick(695),key=695, & 
& nodecnc=[1,2209,2210,2,2573,4781,4782,2574,5146, 5145,5164, 5163,14264, 14263,14345, 14346,5154, 5153 & 
& ,5172, 5171,14272, 14271,14347, 14348], & 
& edgecnc=[1,10,4560,4601,5,14,4564,4602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(695),elname="xbrick",eltype="xbrick",typekey=695) 

        call prepare(lib_xbrick(696),key=696, & 
& nodecnc=[558,565,511,523,3130,3137,3083,3095,14349, 14350,8718, 8717,5228, 5227,14156, 14155,14351, 14352 & 
& ,8724, 8723,5236, 5235,14164, 14163], & 
& edgecnc=[4603,1787,42,4506,4604,1790,46,4510], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(696),elname="xbrick",eltype="xbrick",typekey=696) 

        call prepare(lib_xbrick(697),key=697, & 
& nodecnc=[559,564,531,524,3131,3136,3103,3096,14353, 14354,14194, 14193,14336, 14335,14355, 14356,14357 & 
& , 14358,14200, 14199,14340, 14339,14359, 14360], & 
& edgecnc=[4605,4525,4596,4606,4607,4528,4598,4608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(697),elname="xbrick",eltype="xbrick",typekey=697) 

        call prepare(lib_xbrick(698),key=698, & 
& nodecnc=[562,571,534,525,3134,3143,3106,3097,14361, 14362,11470, 11469,14208, 14207,14363, 14364,14365 & 
& , 14366,11476, 11475,14212, 14211,14367, 14368], & 
& edgecnc=[4609,3163,4532,4610,4611,3166,4534,4612], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(698),elname="xbrick",eltype="xbrick",typekey=698) 

        call prepare(lib_xbrick(699),key=699, & 
& nodecnc=[2209,526,570,538,4781,3098,3142,3110,5152, 5151,14369, 14370,14371, 14372,5166, 5165,5160, 5159 & 
& ,14373, 14374,14375, 14376,5174, 5173], & 
& edgecnc=[4,4613,4614,11,8,4615,4616,15], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(699),elname="xbrick",eltype="xbrick",typekey=699) 

        call prepare(lib_xbrick(700),key=700, & 
& nodecnc=[547,566,528,527,3119,3138,3100,3099,14094, 14093,14377, 14378,13138, 13137,14114, 14113,14102 & 
& , 14101,14379, 14380,13144, 13143,14120, 14119], & 
& edgecnc=[4475,4617,3997,4485,4479,4618,4000,4488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(700),elname="xbrick",eltype="xbrick",typekey=700) 

        call prepare(lib_xbrick(701),key=701, & 
& nodecnc=[528,566,563,522,3100,3138,3135,3094,14378, 14377,14381, 14382,14144, 14143,13146, 13145,14380 & 
& , 14379,14383, 14384,14148, 14147,13150, 13149], & 
& edgecnc=[4617,4619,4500,4001,4618,4620,4502,4003], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(701),elname="xbrick",eltype="xbrick",typekey=701) 

        call prepare(lib_xbrick(702),key=702, & 
& nodecnc=[529,2215,568,509,3101,4787,3140,3081,5210, 5209,14385, 14386,14250, 14249,5200, 5199,5216, 5215 & 
& ,14387, 14388,14254, 14253,5208, 5207], & 
& edgecnc=[33,4621,4553,28,36,4622,4555,32], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(702),elname="xbrick",eltype="xbrick",typekey=702) 

        call prepare(lib_xbrick(703),key=703, & 
& nodecnc=[524,521,557,559,3096,3093,3129,3131,5426, 5425,14342, 14341,14389, 14390,14356, 14355,5432 & 
& , 5431,14344, 14343,14391, 14392,14360, 14359], & 
& edgecnc=[141,4599,4623,4606,144,4600,4624,4608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(703),elname="xbrick",eltype="xbrick",typekey=703) 

        call prepare(lib_xbrick(704),key=704, & 
& nodecnc=[555,533,503,540,3127,3105,3075,3112,5270, 5269,14168, 14167,8716, 8715,14393, 14394,5276, 5275 & 
& ,14172, 14171,8722, 8721,14395, 14396], & 
& edgecnc=[63,4512,1786,4625,66,4514,1789,4626], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(704),elname="xbrick",eltype="xbrick",typekey=704) 

        call prepare(lib_xbrick(705),key=705, & 
& nodecnc=[517,513,576,556,3089,3085,3148,3128,5334, 5333,11474, 11473,8626, 8625,14294, 14293,5338, 5337 & 
& ,11480, 11479,8634, 8633,14300, 14299], & 
& edgecnc=[95,3165,1741,4575,97,3168,1745,4578], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(705),elname="xbrick",eltype="xbrick",typekey=705) 

        call prepare(lib_xbrick(706),key=706, & 
& nodecnc=[525,506,2217,562,3097,3078,4789,3134,5290, 5289,14397, 14398,14399, 14400,14364, 14363,5296 & 
& , 5295,14401, 14402,14403, 14404,14368, 14367], & 
& edgecnc=[73,4627,4628,4610,76,4629,4630,4612], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(706),elname="xbrick",eltype="xbrick",typekey=706) 

        call prepare(lib_xbrick(707),key=707, & 
& nodecnc=[498,537,570,526,3070,3109,3142,3098,5508, 5507,14405, 14406,14370, 14369,14072, 14071,5516 & 
& , 5515,14407, 14408,14374, 14373,14076, 14075], & 
& edgecnc=[182,4631,4613,4464,186,4632,4615,4466], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(707),elname="xbrick",eltype="xbrick",typekey=707) 

        call prepare(lib_xbrick(708),key=708, & 
& nodecnc=[606,2264,582,580,3178,4836,3154,3152,14409, 14410,14411, 14412,14413, 14414,14415, 14416,14417 & 
& , 14418,14419, 14420,14421, 14422,14423, 14424], & 
& edgecnc=[4633,4634,4635,4636,4637,4638,4639,4640], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(708),elname="xbrick",eltype="xbrick",typekey=708) 

        call prepare(lib_xbrick(709),key=709, & 
& nodecnc=[570,581,577,538,3142,3153,3149,3110,14425, 14426,14427, 14428,14429, 14430,14372, 14371,14431 & 
& , 14432,14433, 14434,14435, 14436,14376, 14375], & 
& edgecnc=[4641,4642,4643,4614,4644,4645,4646,4616], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(709),elname="xbrick",eltype="xbrick",typekey=709) 

        call prepare(lib_xbrick(710),key=710, & 
& nodecnc=[539,2223,590,2215,3111,4795,3162,4787,14437, 14438,14439, 14440,14441, 14442,5214, 5213,14443 & 
& , 14444,14445, 14446,14447, 14448,5220, 5219], & 
& edgecnc=[4647,4648,4649,35,4650,4651,4652,38], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(710),elname="xbrick",eltype="xbrick",typekey=710) 

        call prepare(lib_xbrick(711),key=711, & 
& nodecnc=[542,593,2223,539,3114,3165,4795,3111,14449, 14450,14451, 14452,14438, 14437,5180, 5179,14453 & 
& , 14454,14455, 14456,14444, 14443,5188, 5187], & 
& edgecnc=[4653,4654,4647,18,4655,4656,4650,22], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(711),elname="xbrick",eltype="xbrick",typekey=711) 

        call prepare(lib_xbrick(712),key=712, & 
& nodecnc=[632,625,600,601,3204,3197,3172,3173,14457, 14458,14459, 14460,14461, 14462,14463, 14464,14465 & 
& , 14466,14467, 14468,14469, 14470,14471, 14472], & 
& edgecnc=[4657,4658,4659,4660,4661,4662,4663,4664], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(712),elname="xbrick",eltype="xbrick",typekey=712) 

        call prepare(lib_xbrick(713),key=713, & 
& nodecnc=[2256,541,568,589,4828,3113,3140,3161,14473, 14474,14252, 14251,14475, 14476,14477, 14478,14479 & 
& , 14480,14256, 14255,14481, 14482,14483, 14484], & 
& edgecnc=[4665,4554,4666,4667,4668,4556,4669,4670], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(713),elname="xbrick",eltype="xbrick",typekey=713) 

        call prepare(lib_xbrick(714),key=714, & 
& nodecnc=[2216,561,578,585,4788,3133,3150,3157,14324, 14323,14485, 14486,14487, 14488,14489, 14490,14330 & 
& , 14329,14491, 14492,14493, 14494,14495, 14496], & 
& edgecnc=[4590,4671,4672,4673,4593,4674,4675,4676], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(714),elname="xbrick",eltype="xbrick",typekey=714) 

        call prepare(lib_xbrick(715),key=715, & 
& nodecnc=[123,580,543,563,2695,3152,3115,3135,14497, 14498,14499, 14500,14142, 14141,14501, 14502,14503 & 
& , 14504,14505, 14506,14146, 14145,14507, 14508], & 
& edgecnc=[4677,4678,4499,4679,4680,4681,4501,4682], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(715),elname="xbrick",eltype="xbrick",typekey=715) 

        call prepare(lib_xbrick(716),key=716, & 
& nodecnc=[2241,595,660,619,4813,3167,3232,3191,14509, 14510,14511, 14512,14513, 14514,14515, 14516,14517 & 
& , 14518,14519, 14520,14521, 14522,14523, 14524], & 
& edgecnc=[4683,4684,4685,4686,4687,4688,4689,4690], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(716),elname="xbrick",eltype="xbrick",typekey=716) 

        call prepare(lib_xbrick(717),key=717, & 
& nodecnc=[545,550,500,505,3117,3122,3072,3077,14525, 14526,14110, 14109,5454, 5453,14198, 14197,14527 & 
& , 14528,14116, 14115,5460, 5459,14204, 14203], & 
& edgecnc=[4691,4483,155,4527,4692,4486,158,4530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(717),elname="xbrick",eltype="xbrick",typekey=717) 

        call prepare(lib_xbrick(718),key=718, & 
& nodecnc=[637,605,564,603,3209,3177,3136,3175,14529, 14530,14531, 14532,14533, 14534,14535, 14536,14537 & 
& , 14538,14539, 14540,14541, 14542,14543, 14544], & 
& edgecnc=[4693,4694,4695,4696,4697,4698,4699,4700], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(718),elname="xbrick",eltype="xbrick",typekey=718) 

        call prepare(lib_xbrick(719),key=719, & 
& nodecnc=[579,546,551,174,3151,3118,3123,2746,14545, 14546,14302, 14301,14547, 14548,8644, 8643,14549 & 
& , 14550,14306, 14305,14551, 14552,8652, 8651], & 
& edgecnc=[4701,4579,4702,1750,4703,4581,4704,1754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(719),elname="xbrick",eltype="xbrick",typekey=719) 

        call prepare(lib_xbrick(720),key=720, & 
& nodecnc=[546,567,532,514,3118,3139,3104,3086,14553, 14554,14218, 14217,5364, 5363,14304, 14303,14555 & 
& , 14556,14224, 14223,5372, 5371,14308, 14307], & 
& edgecnc=[4705,4537,110,4580,4706,4540,114,4582], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(720),elname="xbrick",eltype="xbrick",typekey=720) 

        call prepare(lib_xbrick(721),key=721, & 
& nodecnc=[643,583,566,586,3215,3155,3138,3158,14557, 14558,14559, 14560,14100, 14099,14561, 14562,14563 & 
& , 14564,14565, 14566,14108, 14107,14567, 14568], & 
& edgecnc=[4707,4708,4478,4709,4710,4711,4482,4712], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(721),elname="xbrick",eltype="xbrick",typekey=721) 

        call prepare(lib_xbrick(722),key=722, & 
& nodecnc=[538,577,94,548,3110,3149,2666,3120,14430, 14429,14569, 14570,14571, 14572,5168, 5167,14436 & 
& , 14435,14573, 14574,14575, 14576,5176, 5175], & 
& edgecnc=[4643,4713,4714,12,4646,4715,4716,16], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(722),elname="xbrick",eltype="xbrick",typekey=722) 

        call prepare(lib_xbrick(723),key=723, & 
& nodecnc=[508,507,552,554,3080,3079,3124,3126,5386, 5385,14222, 14221,14577, 14578,6700, 6699,5392, 5391 & 
& ,14228, 14227,14579, 14580,6708, 6707], & 
& edgecnc=[121,4539,4717,778,124,4542,4718,782], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(723),elname="xbrick",eltype="xbrick",typekey=723) 

        call prepare(lib_xbrick(724),key=724, & 
& nodecnc=[547,550,602,592,3119,3122,3174,3164,14112, 14111,14581, 14582,14583, 14584,14096, 14095,14118 & 
& , 14117,14585, 14586,14587, 14588,14104, 14103], & 
& edgecnc=[4484,4719,4720,4476,4487,4721,4722,4480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(724),elname="xbrick",eltype="xbrick",typekey=724) 

        call prepare(lib_xbrick(725),key=725, & 
& nodecnc=[530,535,572,574,3102,3107,3144,3146,13126, 13125,14292, 14291,14589, 14590,14178, 14177,13130 & 
& , 13129,14298, 14297,14591, 14592,14184, 14183], & 
& edgecnc=[3991,4574,4723,4517,3993,4577,4724,4520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(725),elname="xbrick",eltype="xbrick",typekey=725) 

        call prepare(lib_xbrick(726),key=726, & 
& nodecnc=[623,2272,587,2242,3195,4844,3159,4814,14593, 14594,14595, 14596,14597, 14598,14599, 14600,14601 & 
& , 14602,14603, 14604,14605, 14606,14607, 14608], & 
& edgecnc=[4725,4726,4727,4728,4729,4730,4731,4732], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(726),elname="xbrick",eltype="xbrick",typekey=726) 

        call prepare(lib_xbrick(727),key=727, & 
& nodecnc=[2241,544,549,595,4813,3116,3121,3167,14232, 14231,14310, 14309,14609, 14610,14510, 14509,14240 & 
& , 14239,14314, 14313,14611, 14612,14518, 14517], & 
& edgecnc=[4544,4583,4733,4683,4548,4585,4734,4687], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(727),elname="xbrick",eltype="xbrick",typekey=727) 

        call prepare(lib_xbrick(728),key=728, & 
& nodecnc=[554,552,587,594,3126,3124,3159,3166,14578, 14577,14613, 14614,14615, 14616,14617, 14618,14580 & 
& , 14579,14619, 14620,14621, 14622,14623, 14624], & 
& edgecnc=[4717,4735,4736,4737,4718,4738,4739,4740], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(728),elname="xbrick",eltype="xbrick",typekey=728) 

        call prepare(lib_xbrick(729),key=729, & 
& nodecnc=[573,555,600,611,3145,3127,3172,3183,5266, 5265,14625, 14626,14627, 14628,14629, 14630,5272 & 
& , 5271,14631, 14632,14633, 14634,14635, 14636], & 
& edgecnc=[61,4741,4742,4743,64,4744,4745,4746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(729),elname="xbrick",eltype="xbrick",typekey=729) 

        call prepare(lib_xbrick(730),key=730, & 
& nodecnc=[651,609,612,641,3223,3181,3184,3213,14637, 14638,14639, 14640,14641, 14642,14643, 14644,14645 & 
& , 14646,14647, 14648,14649, 14650,14651, 14652], & 
& edgecnc=[4747,4748,4749,4750,4751,4752,4753,4754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(730),elname="xbrick",eltype="xbrick",typekey=730) 

        call prepare(lib_xbrick(731),key=731, & 
& nodecnc=[588,591,559,557,3160,3163,3131,3129,14653, 14654,14655, 14656,14390, 14389,14236, 14235,14657 & 
& , 14658,14659, 14660,14392, 14391,14244, 14243], & 
& edgecnc=[4755,4756,4623,4546,4757,4758,4624,4550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(731),elname="xbrick",eltype="xbrick",typekey=731) 

        call prepare(lib_xbrick(732),key=732, & 
& nodecnc=[598,558,541,2256,3170,3130,3113,4828,14661, 14662,14154, 14153,14474, 14473,14663, 14664,14665 & 
& , 14666,14162, 14161,14480, 14479,14667, 14668], & 
& edgecnc=[4759,4505,4665,4760,4761,4509,4668,4762], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(732),elname="xbrick",eltype="xbrick",typekey=732) 

        call prepare(lib_xbrick(733),key=733, & 
& nodecnc=[560,597,593,542,3132,3169,3165,3114,14669, 14670,14671, 14672,14450, 14449,14286, 14285,14673 & 
& , 14674,14675, 14676,14454, 14453,14288, 14287], & 
& edgecnc=[4763,4764,4653,4571,4765,4766,4655,4572], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(733),elname="xbrick",eltype="xbrick",typekey=733) 

        call prepare(lib_xbrick(734),key=734, & 
& nodecnc=[585,631,596,553,3157,3203,3168,3125,14677, 14678,14679, 14680,14128, 14127,14681, 14682,14683 & 
& , 14684,14685, 14686,14136, 14135,14687, 14688], & 
& edgecnc=[4767,4768,4492,4769,4770,4771,4496,4772], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(734),elname="xbrick",eltype="xbrick",typekey=734) 

        call prepare(lib_xbrick(735),key=735, & 
& nodecnc=[94,578,561,548,2666,3150,3133,3120,14689, 14690,14486, 14485,14691, 14692,14572, 14571,14693 & 
& , 14694,14492, 14491,14695, 14696,14576, 14575], & 
& edgecnc=[4773,4671,4774,4714,4775,4674,4776,4716], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(735),elname="xbrick",eltype="xbrick",typekey=735) 

        call prepare(lib_xbrick(736),key=736, & 
& nodecnc=[2210,548,561,510,4782,3120,3133,3082,5162, 5161,14692, 14691,14322, 14321,14258, 14257,5170 & 
& , 5169,14696, 14695,14328, 14327,14266, 14265], & 
& edgecnc=[9,4774,4589,4557,13,4776,4592,4561], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(736),elname="xbrick",eltype="xbrick",typekey=736) 

        call prepare(lib_xbrick(737),key=737, & 
& nodecnc=[639,575,536,573,3211,3147,3108,3145,14697, 14698,14699, 14700,5268, 5267,14701, 14702,14703 & 
& , 14704,14705, 14706,5274, 5273,14707, 14708], & 
& edgecnc=[4777,4778,62,4779,4780,4781,65,4782], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(737),elname="xbrick",eltype="xbrick",typekey=737) 

        call prepare(lib_xbrick(738),key=738, & 
& nodecnc=[566,583,123,563,3138,3155,2695,3135,14560, 14559,14709, 14710,14502, 14501,14382, 14381,14566 & 
& , 14565,14711, 14712,14508, 14507,14384, 14383], & 
& edgecnc=[4708,4783,4679,4619,4711,4784,4682,4620], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(738),elname="xbrick",eltype="xbrick",typekey=738) 

        call prepare(lib_xbrick(739),key=739, & 
& nodecnc=[564,605,2240,545,3136,3177,4812,3117,14532, 14531,14713, 14714,14715, 14716,14196, 14195,14540 & 
& , 14539,14717, 14718,14719, 14720,14202, 14201], & 
& edgecnc=[4694,4785,4786,4526,4698,4787,4788,4529], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(739),elname="xbrick",eltype="xbrick",typekey=739) 

        call prepare(lib_xbrick(740),key=740, & 
& nodecnc=[555,540,601,600,3127,3112,3173,3172,14394, 14393,14721, 14722,14462, 14461,14626, 14625,14396 & 
& , 14395,14723, 14724,14470, 14469,14632, 14631], & 
& edgecnc=[4625,4789,4659,4741,4626,4790,4663,4744], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(740),elname="xbrick",eltype="xbrick",typekey=740) 

        call prepare(lib_xbrick(741),key=741, & 
& nodecnc=[604,601,540,565,3176,3173,3112,3137,14725, 14726,14722, 14721,8714, 8713,14727, 14728,14729 & 
& , 14730,14724, 14723,8720, 8719,14731, 14732], & 
& edgecnc=[4791,4789,1785,4792,4793,4790,1788,4794], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(741),elname="xbrick",eltype="xbrick",typekey=741) 

        call prepare(lib_xbrick(742),key=742, & 
& nodecnc=[2215,590,589,568,4787,3162,3161,3140,14442, 14441,14733, 14734,14476, 14475,14386, 14385,14448 & 
& , 14447,14735, 14736,14482, 14481,14388, 14387], & 
& edgecnc=[4649,4795,4666,4621,4652,4796,4669,4622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(742),elname="xbrick",eltype="xbrick",typekey=742) 

        call prepare(lib_xbrick(743),key=743, & 
& nodecnc=[581,570,537,2248,3153,3142,3109,4820,14426, 14425,14406, 14405,14737, 14738,14078, 14077,14432 & 
& , 14431,14408, 14407,14739, 14740,14086, 14085], & 
& edgecnc=[4641,4631,4797,4467,4644,4632,4798,4471], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(743),elname="xbrick",eltype="xbrick",typekey=743) 

        call prepare(lib_xbrick(744),key=744, & 
& nodecnc=[569,543,580,582,3141,3115,3152,3154,6584, 6583,14500, 14499,14414, 14413,14741, 14742,6588 & 
& , 6587,14506, 14505,14422, 14421,14743, 14744], & 
& edgecnc=[720,4678,4635,4799,722,4681,4639,4800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(744),elname="xbrick",eltype="xbrick",typekey=744) 

        call prepare(lib_xbrick(745),key=745, & 
& nodecnc=[2247,614,94,577,4819,3186,2666,3149,14745, 14746,5526, 5525,14570, 14569,14747, 14748,14749 & 
& , 14750,5534, 5533,14574, 14573,14751, 14752], & 
& edgecnc=[4801,191,4713,4802,4803,195,4715,4804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(745),elname="xbrick",eltype="xbrick",typekey=745) 

        call prepare(lib_xbrick(746),key=746, & 
& nodecnc=[157,642,571,562,2729,3214,3143,3134,14753, 14754,14755, 14756,14362, 14361,14757, 14758,14759 & 
& , 14760,14761, 14762,14366, 14365,14763, 14764], & 
& edgecnc=[4805,4806,4609,4807,4808,4809,4611,4810], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(746),elname="xbrick",eltype="xbrick",typekey=746) 

        call prepare(lib_xbrick(747),key=747, & 
& nodecnc=[692,646,647,683,3264,3218,3219,3255,8334, 8333,14765, 14766,14767, 14768,14769, 14770,8342 & 
& , 8341,14771, 14772,14773, 14774,14775, 14776], & 
& edgecnc=[1595,4811,4812,4813,1599,4814,4815,4816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(747),elname="xbrick",eltype="xbrick",typekey=747) 

        call prepare(lib_xbrick(748),key=748, & 
& nodecnc=[2217,506,536,575,4789,3078,3108,3147,14398, 14397,5278, 5277,14700, 14699,14777, 14778,14402 & 
& , 14401,5284, 5283,14706, 14705,14779, 14780], & 
& edgecnc=[4627,67,4778,4817,4629,70,4781,4818], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(748),elname="xbrick",eltype="xbrick",typekey=748) 

        call prepare(lib_xbrick(749),key=749, & 
& nodecnc=[674,639,573,611,3246,3211,3145,3183,14781, 14782,14702, 14701,14630, 14629,14783, 14784,14785 & 
& , 14786,14708, 14707,14636, 14635,14787, 14788], & 
& edgecnc=[4819,4779,4743,4820,4821,4782,4746,4822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(749),elname="xbrick",eltype="xbrick",typekey=749) 

        call prepare(lib_xbrick(750),key=750, & 
& nodecnc=[609,638,574,572,3181,3210,3146,3144,14789, 14790,14791, 14792,14590, 14589,14793, 14794,14795 & 
& , 14796,14797, 14798,14592, 14591,14799, 14800], & 
& edgecnc=[4823,4824,4723,4825,4826,4827,4724,4828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(750),elname="xbrick",eltype="xbrick",typekey=750) 

        call prepare(lib_xbrick(751),key=751, & 
& nodecnc=[562,2217,610,157,3134,4789,3182,2729,14400, 14399,14801, 14802,14803, 14804,14758, 14757,14404 & 
& , 14403,14805, 14806,14807, 14808,14764, 14763], & 
& edgecnc=[4628,4829,4830,4807,4630,4831,4832,4810], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(751),elname="xbrick",eltype="xbrick",typekey=751) 

        call prepare(lib_xbrick(752),key=752, & 
& nodecnc=[157,610,653,684,2729,3182,3225,3256,14804, 14803,14809, 14810,14811, 14812,14813, 14814,14808 & 
& , 14807,14815, 14816,14817, 14818,14819, 14820], & 
& edgecnc=[4830,4833,4834,4835,4832,4836,4837,4838], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(752),elname="xbrick",eltype="xbrick",typekey=752) 

        call prepare(lib_xbrick(753),key=753, & 
& nodecnc=[572,556,612,609,3144,3128,3184,3181,14290, 14289,8624, 8623,14640, 14639,14794, 14793,14296 & 
& , 14295,8632, 8631,14648, 14647,14800, 14799], & 
& edgecnc=[4573,1740,4748,4825,4576,1744,4752,4828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(753),elname="xbrick",eltype="xbrick",typekey=753) 

        call prepare(lib_xbrick(754),key=754, & 
& nodecnc=[2242,567,546,579,4814,3139,3118,3151,14821, 14822,14554, 14553,14546, 14545,14823, 14824,14825 & 
& , 14826,14556, 14555,14550, 14549,14827, 14828], & 
& edgecnc=[4839,4705,4701,4840,4841,4706,4703,4842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(754),elname="xbrick",eltype="xbrick",typekey=754) 

        call prepare(lib_xbrick(755),key=755, & 
& nodecnc=[607,650,585,578,3179,3222,3157,3150,14829, 14830,14831, 14832,14488, 14487,14833, 14834,14835 & 
& , 14836,14837, 14838,14494, 14493,14839, 14840], & 
& edgecnc=[4843,4844,4672,4845,4846,4847,4675,4848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(755),elname="xbrick",eltype="xbrick",typekey=755) 

        call prepare(lib_xbrick(756),key=756, & 
& nodecnc=[2242,587,552,567,4814,3159,3124,3139,14598, 14597,14614, 14613,14220, 14219,14822, 14821,14606 & 
& , 14605,14620, 14619,14226, 14225,14826, 14825], & 
& edgecnc=[4727,4735,4538,4839,4731,4738,4541,4841], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(756),elname="xbrick",eltype="xbrick",typekey=756) 

        call prepare(lib_xbrick(757),key=757, & 
& nodecnc=[649,623,2242,579,3221,3195,4814,3151,14841, 14842,14600, 14599,14824, 14823,8642, 8641,14843 & 
& , 14844,14608, 14607,14828, 14827,8650, 8649], & 
& edgecnc=[4849,4728,4840,1749,4850,4732,4842,1753], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(757),elname="xbrick",eltype="xbrick",typekey=757) 

        call prepare(lib_xbrick(758),key=758, & 
& nodecnc=[626,629,673,672,3198,3201,3245,3244,14082, 14081,14845, 14846,14847, 14848,14849, 14850,14090 & 
& , 14089,14851, 14852,14853, 14854,14855, 14856], & 
& edgecnc=[4469,4851,4852,4853,4473,4854,4855,4856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(758),elname="xbrick",eltype="xbrick",typekey=758) 

        call prepare(lib_xbrick(759),key=759, & 
& nodecnc=[582,584,537,569,3154,3156,3109,3141,14857, 14858,14859, 14860,5506, 5505,14742, 14741,14861 & 
& , 14862,14863, 14864,5514, 5513,14744, 14743], & 
& edgecnc=[4857,4858,181,4799,4859,4860,185,4800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(759),elname="xbrick",eltype="xbrick",typekey=759) 

        call prepare(lib_xbrick(760),key=760, & 
& nodecnc=[2264,624,584,582,4836,3196,3156,3154,14865, 14866,14867, 14868,14858, 14857,14412, 14411,14869 & 
& , 14870,14871, 14872,14862, 14861,14420, 14419], & 
& edgecnc=[4861,4862,4857,4634,4863,4864,4859,4638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(760),elname="xbrick",eltype="xbrick",typekey=760) 

        call prepare(lib_xbrick(761),key=761, & 
& nodecnc=[2265,617,583,643,4837,3189,3155,3215,14873, 14874,14875, 14876,14558, 14557,12036, 12035,14877 & 
& , 14878,14879, 14880,14564, 14563,12044, 12043], & 
& edgecnc=[4865,4866,4707,3446,4867,4868,4710,3450], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(761),elname="xbrick",eltype="xbrick",typekey=761) 

        call prepare(lib_xbrick(762),key=762, & 
& nodecnc=[648,123,583,617,3220,2695,3155,3189,14881, 14882,14710, 14709,14876, 14875,14883, 14884,14885 & 
& , 14886,14712, 14711,14880, 14879,14887, 14888], & 
& edgecnc=[4869,4783,4866,4870,4871,4784,4868,4872], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(762),elname="xbrick",eltype="xbrick",typekey=762) 

        call prepare(lib_xbrick(763),key=763, & 
& nodecnc=[577,581,626,2247,3149,3153,3198,4819,14428, 14427,14084, 14083,14889, 14890,14748, 14747,14434 & 
& , 14433,14092, 14091,14891, 14892,14752, 14751], & 
& edgecnc=[4642,4470,4873,4802,4645,4474,4874,4804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(763),elname="xbrick",eltype="xbrick",typekey=763) 

        call prepare(lib_xbrick(764),key=764, & 
& nodecnc=[537,584,629,2248,3109,3156,3201,4820,14860, 14859,14893, 14894,14080, 14079,14738, 14737,14864 & 
& , 14863,14895, 14896,14088, 14087,14740, 14739], & 
& edgecnc=[4858,4875,4468,4797,4860,4876,4472,4798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(764),elname="xbrick",eltype="xbrick",typekey=764) 

        call prepare(lib_xbrick(765),key=765, & 
& nodecnc=[634,595,549,599,3206,3167,3121,3171,14897, 14898,14610, 14609,14899, 14900,14901, 14902,14903 & 
& , 14904,14612, 14611,14905, 14906,14907, 14908], & 
& edgecnc=[4877,4733,4878,4879,4880,4734,4881,4882], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(765),elname="xbrick",eltype="xbrick",typekey=765) 

        call prepare(lib_xbrick(766),key=766, & 
& nodecnc=[2257,616,2256,589,4829,3188,4828,3161,14909, 14910,14911, 14912,14478, 14477,14913, 14914,14915 & 
& , 14916,14917, 14918,14484, 14483,14919, 14920], & 
& edgecnc=[4883,4884,4667,4885,4886,4887,4670,4888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(766),elname="xbrick",eltype="xbrick",typekey=766) 

        call prepare(lib_xbrick(767),key=767, & 
& nodecnc=[2249,620,658,2274,4821,3192,3230,4846,14921, 14922,14923, 14924,14925, 14926,14927, 14928,14929 & 
& , 14930,14931, 14932,14933, 14934,14935, 14936], & 
& edgecnc=[4889,4890,4891,4892,4893,4894,4895,4896], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(767),elname="xbrick",eltype="xbrick",typekey=767) 

        call prepare(lib_xbrick(768),key=768, & 
& nodecnc=[2223,621,615,590,4795,3193,3187,3162,14937, 14938,14939, 14940,14941, 14942,14440, 14439,14943 & 
& , 14944,14945, 14946,14947, 14948,14446, 14445], & 
& edgecnc=[4897,4898,4899,4648,4900,4901,4902,4651], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(768),elname="xbrick",eltype="xbrick",typekey=768) 

        call prepare(lib_xbrick(769),key=769, & 
& nodecnc=[591,603,564,559,3163,3175,3136,3131,14949, 14950,14534, 14533,14354, 14353,14656, 14655,14951 & 
& , 14952,14542, 14541,14358, 14357,14660, 14659], & 
& edgecnc=[4903,4695,4605,4756,4904,4699,4607,4758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(769),elname="xbrick",eltype="xbrick",typekey=769) 

        call prepare(lib_xbrick(770),key=770, & 
& nodecnc=[613,622,603,591,3185,3194,3175,3163,14953, 14954,14955, 14956,14950, 14949,14957, 14958,14959 & 
& , 14960,14961, 14962,14952, 14951,14963, 14964], & 
& edgecnc=[4905,4906,4903,4907,4908,4909,4904,4910], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(770),elname="xbrick",eltype="xbrick",typekey=770) 

        call prepare(lib_xbrick(771),key=771, & 
& nodecnc=[586,592,627,618,3158,3164,3199,3190,14098, 14097,14965, 14966,14967, 14968,14969, 14970,14106 & 
& , 14105,14971, 14972,14973, 14974,14975, 14976], & 
& edgecnc=[4477,4911,4912,4913,4481,4914,4915,4916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(771),elname="xbrick",eltype="xbrick",typekey=771) 

        call prepare(lib_xbrick(772),key=772, & 
& nodecnc=[596,633,597,560,3168,3205,3169,3132,14977, 14978,14979, 14980,14670, 14669,14122, 14121,14981 & 
& , 14982,14983, 14984,14674, 14673,14130, 14129], & 
& edgecnc=[4917,4918,4763,4489,4919,4920,4765,4493], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(772),elname="xbrick",eltype="xbrick",typekey=772) 

        call prepare(lib_xbrick(773),key=773, & 
& nodecnc=[594,599,549,554,3166,3171,3121,3126,14985, 14986,14900, 14899,6702, 6701,14618, 14617,14987 & 
& , 14988,14906, 14905,6710, 6709,14624, 14623], & 
& edgecnc=[4921,4878,779,4737,4922,4881,783,4740], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(773),elname="xbrick",eltype="xbrick",typekey=773) 

        call prepare(lib_xbrick(774),key=774, & 
& nodecnc=[599,594,630,628,3171,3166,3202,3200,14986, 14985,14989, 14990,14991, 14992,14993, 14994,14988 & 
& , 14987,14995, 14996,14997, 14998,14999, 15000], & 
& edgecnc=[4921,4923,4924,4925,4922,4926,4927,4928], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(774),elname="xbrick",eltype="xbrick",typekey=774) 

        call prepare(lib_xbrick(775),key=775, & 
& nodecnc=[588,2241,619,2270,3160,4813,3191,4842,14234, 14233,14516, 14515,15001, 15002,15003, 15004,14242 & 
& , 14241,14524, 14523,15005, 15006,15007, 15008], & 
& edgecnc=[4545,4686,4929,4930,4549,4690,4931,4932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(775),elname="xbrick",eltype="xbrick",typekey=775) 

        call prepare(lib_xbrick(776),key=776, & 
& nodecnc=[628,2271,634,599,3200,4843,3206,3171,15009, 15010,15011, 15012,14902, 14901,14994, 14993,15013 & 
& , 15014,15015, 15016,14908, 14907,15000, 14999], & 
& edgecnc=[4933,4934,4879,4925,4935,4936,4882,4928], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(776),elname="xbrick",eltype="xbrick",typekey=776) 

        call prepare(lib_xbrick(777),key=777, & 
& nodecnc=[679,631,585,650,3251,3203,3157,3222,15017, 15018,14678, 14677,14832, 14831,15019, 15020,15021 & 
& , 15022,14684, 14683,14838, 14837,15023, 15024], & 
& edgecnc=[4937,4767,4844,4938,4939,4770,4847,4940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(777),elname="xbrick",eltype="xbrick",typekey=777) 

        call prepare(lib_xbrick(778),key=778, & 
& nodecnc=[2249,593,597,620,4821,3165,3169,3192,15025, 15026,14672, 14671,15027, 15028,14922, 14921,15029 & 
& , 15030,14676, 14675,15031, 15032,14930, 14929], & 
& edgecnc=[4941,4764,4942,4889,4943,4766,4944,4893], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(778),elname="xbrick",eltype="xbrick",typekey=778) 

        call prepare(lib_xbrick(779),key=779, & 
& nodecnc=[633,663,620,597,3205,3235,3192,3169,15033, 15034,15035, 15036,15028, 15027,14980, 14979,15037 & 
& , 15038,15039, 15040,15032, 15031,14984, 14983], & 
& edgecnc=[4945,4946,4942,4918,4947,4948,4944,4920], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(779),elname="xbrick",eltype="xbrick",typekey=779) 

        call prepare(lib_xbrick(780),key=780, & 
& nodecnc=[598,604,565,558,3170,3176,3137,3130,15041, 15042,14728, 14727,14350, 14349,14662, 14661,15043 & 
& , 15044,14732, 14731,14352, 14351,14666, 14665], & 
& edgecnc=[4949,4792,4603,4759,4950,4794,4604,4761], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(780),elname="xbrick",eltype="xbrick",typekey=780) 

        call prepare(lib_xbrick(781),key=781, & 
& nodecnc=[662,686,644,625,3234,3258,3216,3197,15045, 15046,15047, 15048,15049, 15050,15051, 15052,15053 & 
& , 15054,15055, 15056,15057, 15058,15059, 15060], & 
& edgecnc=[4951,4952,4953,4954,4955,4956,4957,4958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(781),elname="xbrick",eltype="xbrick",typekey=781) 

        call prepare(lib_xbrick(782),key=782, & 
& nodecnc=[2240,605,671,635,4812,3177,3243,3207,14714, 14713,15061, 15062,15063, 15064,15065, 15066,14718 & 
& , 14717,15067, 15068,15069, 15070,15071, 15072], & 
& edgecnc=[4785,4959,4960,4961,4787,4962,4963,4964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(782),elname="xbrick",eltype="xbrick",typekey=782) 

        call prepare(lib_xbrick(783),key=783, & 
& nodecnc=[2240,602,550,545,4812,3174,3122,3117,15073, 15074,14582, 14581,14526, 14525,14716, 14715,15075 & 
& , 15076,14586, 14585,14528, 14527,14720, 14719], & 
& edgecnc=[4965,4719,4691,4786,4966,4721,4692,4788], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(783),elname="xbrick",eltype="xbrick",typekey=783) 

        call prepare(lib_xbrick(784),key=784, & 
& nodecnc=[604,598,636,2293,3176,3170,3208,4865,15042, 15041,15077, 15078,15079, 15080,15081, 15082,15044 & 
& , 15043,15083, 15084,15085, 15086,15087, 15088], & 
& edgecnc=[4949,4967,4968,4969,4950,4970,4971,4972], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(784),elname="xbrick",eltype="xbrick",typekey=784) 

        call prepare(lib_xbrick(785),key=785, & 
& nodecnc=[592,602,2268,627,3164,3174,4840,3199,14584, 14583,15089, 15090,15091, 15092,14966, 14965,14588 & 
& , 14587,15093, 15094,15095, 15096,14972, 14971], & 
& edgecnc=[4720,4973,4974,4911,4722,4975,4976,4914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(785),elname="xbrick",eltype="xbrick",typekey=785) 

        call prepare(lib_xbrick(786),key=786, & 
& nodecnc=[637,2269,706,670,3209,4841,3278,3242,15097, 15098,15099, 15100,15101, 15102,15103, 15104,15105 & 
& , 15106,15107, 15108,15109, 15110,15111, 15112], & 
& edgecnc=[4977,4978,4979,4980,4981,4982,4983,4984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(786),elname="xbrick",eltype="xbrick",typekey=786) 

        call prepare(lib_xbrick(787),key=787, & 
& nodecnc=[648,606,580,123,3220,3178,3152,2695,6566, 6565,14416, 14415,14498, 14497,14882, 14881,6574 & 
& , 6573,14424, 14423,14504, 14503,14886, 14885], & 
& edgecnc=[711,4636,4677,4869,715,4640,4680,4871], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(787),elname="xbrick",eltype="xbrick",typekey=787) 

        call prepare(lib_xbrick(788),key=788, & 
& nodecnc=[831,784,758,807,3403,3356,3330,3379,15113, 15114,15115, 15116,15117, 15118,15119, 15120,15121 & 
& , 15122,15123, 15124,15125, 15126,15127, 15128], & 
& edgecnc=[4985,4986,4987,4988,4989,4990,4991,4992], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(788),elname="xbrick",eltype="xbrick",typekey=788) 

        call prepare(lib_xbrick(789),key=789, & 
& nodecnc=[645,607,578,94,3217,3179,3150,2666,15129, 15130,14834, 14833,14690, 14689,5524, 5523,15131 & 
& , 15132,14840, 14839,14694, 14693,5532, 5531], & 
& edgecnc=[4993,4845,4773,190,4994,4848,4775,194], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(789),elname="xbrick",eltype="xbrick",typekey=789) 

        call prepare(lib_xbrick(790),key=790, & 
& nodecnc=[2315,678,2346,713,4887,3250,4918,3285,15133, 15134,15135, 15136,15137, 15138,15139, 15140,15141 & 
& , 15142,15143, 15144,15145, 15146,15147, 15148], & 
& edgecnc=[4995,4996,4997,4998,4999,5000,5001,5002], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(790),elname="xbrick",eltype="xbrick",typekey=790) 

        call prepare(lib_xbrick(791),key=791, & 
& nodecnc=[2262,638,609,651,4834,3210,3181,3223,15149, 15150,14790, 14789,14638, 14637,15151, 15152,15153 & 
& , 15154,14796, 14795,14646, 14645,15155, 15156], & 
& edgecnc=[5003,4823,4747,5004,5005,4826,4751,5006], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(791),elname="xbrick",eltype="xbrick",typekey=791) 

        call prepare(lib_xbrick(792),key=792, & 
& nodecnc=[174,551,574,647,2746,3123,3146,3219,14548, 14547,14174, 14173,15157, 15158,15159, 15160,14552 & 
& , 14551,14180, 14179,15161, 15162,15163, 15164], & 
& edgecnc=[4702,4515,5007,5008,4704,4518,5009,5010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(792),elname="xbrick",eltype="xbrick",typekey=792) 

        call prepare(lib_xbrick(793),key=793, & 
& nodecnc=[685,653,575,639,3257,3225,3147,3211,8610, 8609,15165, 15166,14698, 14697,15167, 15168,8618 & 
& , 8617,15169, 15170,14704, 14703,15171, 15172], & 
& edgecnc=[1733,5011,4777,5012,1737,5013,4780,5014], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(793),elname="xbrick",eltype="xbrick",typekey=793) 

        call prepare(lib_xbrick(794),key=794, & 
& nodecnc=[610,2217,575,653,3182,4789,3147,3225,14802, 14801,14778, 14777,15166, 15165,14810, 14809,14806 & 
& , 14805,14780, 14779,15170, 15169,14816, 14815], & 
& edgecnc=[4829,4817,5011,4833,4831,4818,5013,4836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(794),elname="xbrick",eltype="xbrick",typekey=794) 

        call prepare(lib_xbrick(795),key=795, & 
& nodecnc=[611,600,625,644,3183,3172,3197,3216,14628, 14627,14460, 14459,15050, 15049,15173, 15174,14634 & 
& , 14633,14468, 14467,15058, 15057,15175, 15176], & 
& edgecnc=[4742,4658,4953,5015,4745,4662,4957,5016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(795),elname="xbrick",eltype="xbrick",typekey=795) 

        call prepare(lib_xbrick(796),key=796, & 
& nodecnc=[640,661,641,612,3212,3233,3213,3184,15177, 15178,15179, 15180,14642, 14641,8622, 8621,15181 & 
& , 15182,15183, 15184,14650, 14649,8630, 8629], & 
& edgecnc=[5017,5018,4749,1739,5019,5020,4753,1743], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(796),elname="xbrick",eltype="xbrick",typekey=796) 

        call prepare(lib_xbrick(797),key=797, & 
& nodecnc=[642,640,576,571,3214,3212,3148,3143,15185, 15186,8628, 8627,11472, 11471,14756, 14755,15187 & 
& , 15188,8636, 8635,11478, 11477,14762, 14761], & 
& edgecnc=[5021,1742,3164,4806,5022,1746,3167,4809], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(797),elname="xbrick",eltype="xbrick",typekey=797) 

        call prepare(lib_xbrick(798),key=798, & 
& nodecnc=[2349,652,613,2270,4921,3224,3185,4842,15189, 15190,15191, 15192,15193, 15194,15195, 15196,15197 & 
& , 15198,15199, 15200,15201, 15202,15203, 15204], & 
& edgecnc=[5023,5024,5025,5026,5027,5028,5029,5030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(798),elname="xbrick",eltype="xbrick",typekey=798) 

        call prepare(lib_xbrick(799),key=799, & 
& nodecnc=[2270,613,591,588,4842,3185,3163,3160,15194, 15193,14958, 14957,14654, 14653,15004, 15003,15202 & 
& , 15201,14964, 14963,14658, 14657,15008, 15007], & 
& edgecnc=[5025,4907,4755,4930,5029,4910,4757,4932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(799),elname="xbrick",eltype="xbrick",typekey=799) 

        call prepare(lib_xbrick(800),key=800, & 
& nodecnc=[2257,589,590,615,4829,3161,3162,3187,14914, 14913,14734, 14733,14942, 14941,15205, 15206,14920 & 
& , 14919,14736, 14735,14948, 14947,15207, 15208], & 
& edgecnc=[4885,4795,4899,5031,4888,4796,4902,5032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(800),elname="xbrick",eltype="xbrick",typekey=800) 

        call prepare(lib_xbrick(801),key=801, & 
& nodecnc=[654,2275,693,139,3226,4847,3265,2711,15209, 15210,15211, 15212,15213, 15214,15215, 15216,15217 & 
& , 15218,15219, 15220,15221, 15222,15223, 15224], & 
& edgecnc=[5033,5034,5035,5036,5037,5038,5039,5040], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(801),elname="xbrick",eltype="xbrick",typekey=801) 

        call prepare(lib_xbrick(802),key=802, & 
& nodecnc=[2257,654,657,616,4829,3226,3229,3188,15225, 15226,15227, 15228,15229, 15230,14910, 14909,15231 & 
& , 15232,15233, 15234,15235, 15236,14916, 14915], & 
& edgecnc=[5041,5042,5043,4883,5044,5045,5046,4886], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(802),elname="xbrick",eltype="xbrick",typekey=802) 

        call prepare(lib_xbrick(803),key=803, & 
& nodecnc=[618,2267,643,586,3190,4839,3215,3158,15237, 15238,12030, 12029,14562, 14561,14970, 14969,15239 & 
& , 15240,12038, 12037,14568, 14567,14976, 14975], & 
& edgecnc=[5047,3443,4709,4913,5048,3447,4712,4916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(803),elname="xbrick",eltype="xbrick",typekey=803) 

        call prepare(lib_xbrick(804),key=804, & 
& nodecnc=[2311,659,618,627,4883,3231,3190,3199,15241, 15242,15243, 15244,14968, 14967,15245, 15246,15247 & 
& , 15248,15249, 15250,14974, 14973,15251, 15252], & 
& edgecnc=[5049,5050,4912,5051,5052,5053,4915,5054], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(804),elname="xbrick",eltype="xbrick",typekey=804) 

        call prepare(lib_xbrick(805),key=805, & 
& nodecnc=[655,652,731,2347,3227,3224,3303,4919,15253, 15254,15255, 15256,15257, 15258,15259, 15260,15261 & 
& , 15262,15263, 15264,15265, 15266,15267, 15268], & 
& edgecnc=[5055,5056,5057,5058,5059,5060,5061,5062], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(805),elname="xbrick",eltype="xbrick",typekey=805) 

        call prepare(lib_xbrick(806),key=806, & 
& nodecnc=[593,2249,621,2223,3165,4821,3193,4795,15026, 15025,15269, 15270,14938, 14937,14452, 14451,15030 & 
& , 15029,15271, 15272,14944, 14943,14456, 14455], & 
& edgecnc=[4941,5063,4897,4654,4943,5064,4900,4656], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(806),elname="xbrick",eltype="xbrick",typekey=806) 

        call prepare(lib_xbrick(807),key=807, & 
& nodecnc=[665,633,596,631,3237,3205,3168,3203,15273, 15274,14978, 14977,14680, 14679,15275, 15276,15277 & 
& , 15278,14982, 14981,14686, 14685,15279, 15280], & 
& edgecnc=[5065,4917,4768,5066,5067,4919,4771,5068], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(807),elname="xbrick",eltype="xbrick",typekey=807) 

        call prepare(lib_xbrick(808),key=808, & 
& nodecnc=[654,2257,615,2275,3226,4829,3187,4847,15226, 15225,15206, 15205,15281, 15282,15210, 15209,15232 & 
& , 15231,15208, 15207,15283, 15284,15218, 15217], & 
& edgecnc=[5041,5031,5069,5033,5044,5032,5070,5037], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(808),elname="xbrick",eltype="xbrick",typekey=808) 

        call prepare(lib_xbrick(809),key=809, & 
& nodecnc=[721,656,2274,695,3293,3228,4846,3267,15285, 15286,15287, 15288,15289, 15290,15291, 15292,15293 & 
& , 15294,15295, 15296,15297, 15298,15299, 15300], & 
& edgecnc=[5071,5072,5073,5074,5075,5076,5077,5078], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(809),elname="xbrick",eltype="xbrick",typekey=809) 

        call prepare(lib_xbrick(810),key=810, & 
& nodecnc=[2272,630,594,587,4844,3202,3166,3159,15301, 15302,14990, 14989,14616, 14615,14596, 14595,15303 & 
& , 15304,14996, 14995,14622, 14621,14604, 14603], & 
& edgecnc=[5079,4923,4736,4726,5080,4926,4739,4730], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(810),elname="xbrick",eltype="xbrick",typekey=810) 

        call prepare(lib_xbrick(811),key=811, & 
& nodecnc=[682,2316,649,608,3254,4888,3221,3180,15305, 15306,15307, 15308,8640, 8639,15309, 15310,15311 & 
& , 15312,15313, 15314,8648, 8647,15315, 15316], & 
& edgecnc=[5081,5082,1748,5083,5084,5085,1752,5086], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(811),elname="xbrick",eltype="xbrick",typekey=811) 

        call prepare(lib_xbrick(812),key=812, & 
& nodecnc=[676,710,673,624,3248,3282,3245,3196,15317, 15318,15319, 15320,15321, 15322,15323, 15324,15325 & 
& , 15326,15327, 15328,15329, 15330,15331, 15332], & 
& edgecnc=[5087,5088,5089,5090,5091,5092,5093,5094], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(812),elname="xbrick",eltype="xbrick",typekey=812) 

        call prepare(lib_xbrick(813),key=813, & 
& nodecnc=[627,2268,667,2311,3199,4840,3239,4883,15092, 15091,15333, 15334,15335, 15336,15246, 15245,15096 & 
& , 15095,15337, 15338,15339, 15340,15252, 15251], & 
& edgecnc=[4974,5095,5096,5051,4976,5097,5098,5054], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(813),elname="xbrick",eltype="xbrick",typekey=813) 

        call prepare(lib_xbrick(814),key=814, & 
& nodecnc=[628,630,664,666,3200,3202,3236,3238,14992, 14991,15341, 15342,15343, 15344,15345, 15346,14998 & 
& , 14997,15347, 15348,15349, 15350,15351, 15352], & 
& edgecnc=[4924,5099,5100,5101,4927,5102,5103,5104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(814),elname="xbrick",eltype="xbrick",typekey=814) 

        call prepare(lib_xbrick(815),key=815, & 
& nodecnc=[726,675,2266,709,3298,3247,4838,3281,15353, 15354,15355, 15356,15357, 15358,15359, 15360,15361 & 
& , 15362,15363, 15364,15365, 15366,15367, 15368], & 
& edgecnc=[5105,5106,5107,5108,5109,5110,5111,5112], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(815),elname="xbrick",eltype="xbrick",typekey=815) 

        call prepare(lib_xbrick(816),key=816, & 
& nodecnc=[673,629,584,624,3245,3201,3156,3196,14846, 14845,14894, 14893,14868, 14867,15322, 15321,14852 & 
& , 14851,14896, 14895,14872, 14871,15330, 15329], & 
& edgecnc=[4851,4875,4862,5089,4854,4876,4864,5093], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(816),elname="xbrick",eltype="xbrick",typekey=816) 

        call prepare(lib_xbrick(817),key=817, & 
& nodecnc=[2315,664,630,2272,4887,3236,3202,4844,15369, 15370,15342, 15341,15302, 15301,15371, 15372,15373 & 
& , 15374,15348, 15347,15304, 15303,15375, 15376], & 
& edgecnc=[5113,5099,5079,5114,5115,5102,5080,5116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(817),elname="xbrick",eltype="xbrick",typekey=817) 

        call prepare(lib_xbrick(818),key=818, & 
& nodecnc=[714,665,631,679,3286,3237,3203,3251,15377, 15378,15276, 15275,15018, 15017,8848, 8847,15379 & 
& , 15380,15280, 15279,15022, 15021,8856, 8855], & 
& edgecnc=[5117,5066,4937,1852,5118,5068,4939,1856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(818),elname="xbrick",eltype="xbrick",typekey=818) 

        call prepare(lib_xbrick(819),key=819, & 
& nodecnc=[662,625,632,2320,3234,3197,3204,4892,15052, 15051,14458, 14457,15381, 15382,15383, 15384,15060 & 
& , 15059,14466, 14465,15385, 15386,15387, 15388], & 
& edgecnc=[4954,4657,5119,5120,4958,4661,5121,5122], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(819),elname="xbrick",eltype="xbrick",typekey=819) 

        call prepare(lib_xbrick(820),key=820, & 
& nodecnc=[2293,632,601,604,4865,3204,3173,3176,15389, 15390,14464, 14463,14726, 14725,15082, 15081,15391 & 
& , 15392,14472, 14471,14730, 14729,15088, 15087], & 
& edgecnc=[5123,4660,4791,4969,5124,4664,4793,4972], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(820),elname="xbrick",eltype="xbrick",typekey=820) 

        call prepare(lib_xbrick(821),key=821, & 
& nodecnc=[696,658,620,663,3268,3230,3192,3235,15393, 15394,14924, 14923,15036, 15035,15395, 15396,15397 & 
& , 15398,14932, 14931,15040, 15039,15399, 15400], & 
& edgecnc=[5125,4890,4946,5126,5127,4894,4948,5128], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(821),elname="xbrick",eltype="xbrick",typekey=821) 

        call prepare(lib_xbrick(822),key=822, & 
& nodecnc=[699,663,633,665,3271,3235,3205,3237,15401, 15402,15034, 15033,15274, 15273,15403, 15404,15405 & 
& , 15406,15038, 15037,15278, 15277,15407, 15408], & 
& edgecnc=[5129,4945,5065,5130,5131,4947,5067,5132], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(822),elname="xbrick",eltype="xbrick",typekey=822) 

        call prepare(lib_xbrick(823),key=823, & 
& nodecnc=[2270,619,694,2349,4842,3191,3266,4921,15002, 15001,15409, 15410,15411, 15412,15196, 15195,15006 & 
& , 15005,15413, 15414,15415, 15416,15204, 15203], & 
& edgecnc=[4929,5133,5134,5026,4931,5135,5136,5030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(823),elname="xbrick",eltype="xbrick",typekey=823) 

        call prepare(lib_xbrick(824),key=824, & 
& nodecnc=[595,634,668,660,3167,3206,3240,3232,14898, 14897,15417, 15418,15419, 15420,14512, 14511,14904 & 
& , 14903,15421, 15422,15423, 15424,14520, 14519], & 
& edgecnc=[4877,5137,5138,4684,4880,5139,5140,4688], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(824),elname="xbrick",eltype="xbrick",typekey=824) 

        call prepare(lib_xbrick(825),key=825, & 
& nodecnc=[2268,635,2312,667,4840,3207,4884,3239,15425, 15426,15427, 15428,15429, 15430,15334, 15333,15431 & 
& , 15432,15433, 15434,15435, 15436,15338, 15337], & 
& edgecnc=[5141,5142,5143,5095,5144,5145,5146,5097], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(825),elname="xbrick",eltype="xbrick",typekey=825) 

        call prepare(lib_xbrick(826),key=826, & 
& nodecnc=[2339,669,2293,636,4911,3241,4865,3208,8704, 8703,15437, 15438,15080, 15079,15439, 15440,8712 & 
& , 8711,15441, 15442,15086, 15085,15443, 15444], & 
& edgecnc=[1780,5147,4968,5148,1784,5149,4971,5150], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(826),elname="xbrick",eltype="xbrick",typekey=826) 

        call prepare(lib_xbrick(827),key=827, & 
& nodecnc=[2256,616,636,598,4828,3188,3208,3170,14912, 14911,15445, 15446,15078, 15077,14664, 14663,14918 & 
& , 14917,15447, 15448,15084, 15083,14668, 14667], & 
& edgecnc=[4884,5151,4967,4760,4887,5152,4970,4762], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(827),elname="xbrick",eltype="xbrick",typekey=827) 

        call prepare(lib_xbrick(828),key=828, & 
& nodecnc=[637,670,671,605,3209,3242,3243,3177,15104, 15103,15449, 15450,15062, 15061,14530, 14529,15112 & 
& , 15111,15451, 15452,15068, 15067,14538, 14537], & 
& edgecnc=[4980,5153,4959,4693,4984,5154,4962,4697], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(828),elname="xbrick",eltype="xbrick",typekey=828) 

        call prepare(lib_xbrick(829),key=829, & 
& nodecnc=[622,2269,637,603,3194,4841,3209,3175,15453, 15454,15098, 15097,14536, 14535,14956, 14955,15455 & 
& , 15456,15106, 15105,14544, 14543,14962, 14961], & 
& edgecnc=[5155,4977,4696,4906,5156,4981,4700,4909], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(829),elname="xbrick",eltype="xbrick",typekey=829) 

        call prepare(lib_xbrick(830),key=830, & 
& nodecnc=[608,174,647,646,3180,2746,3219,3218,8638, 8637,15160, 15159,14766, 14765,15457, 15458,8646 & 
& , 8645,15164, 15163,14772, 14771,15459, 15460], & 
& edgecnc=[1747,5008,4811,5157,1751,5010,4814,5158], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(830),elname="xbrick",eltype="xbrick",typekey=830) 

        call prepare(lib_xbrick(831),key=831, & 
& nodecnc=[642,681,661,640,3214,3253,3233,3212,15461, 15462,15463, 15464,15178, 15177,15186, 15185,15465 & 
& , 15466,15467, 15468,15182, 15181,15188, 15187], & 
& edgecnc=[5159,5160,5017,5021,5161,5162,5019,5022], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(831),elname="xbrick",eltype="xbrick",typekey=831) 

        call prepare(lib_xbrick(832),key=832, & 
& nodecnc=[2407,690,691,733,4979,3262,3263,3305,15469, 15470,15471, 15472,15473, 15474,15475, 15476,15477 & 
& , 15478,15479, 15480,15481, 15482,15483, 15484], & 
& edgecnc=[5163,5164,5165,5166,5167,5168,5169,5170], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(832),elname="xbrick",eltype="xbrick",typekey=832) 

        call prepare(lib_xbrick(833),key=833, & 
& nodecnc=[661,691,690,641,3233,3263,3262,3213,15485, 15486,15472, 15471,15487, 15488,15180, 15179,15489 & 
& , 15490,15480, 15479,15491, 15492,15184, 15183], & 
& edgecnc=[5171,5164,5172,5018,5173,5168,5174,5020], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(833),elname="xbrick",eltype="xbrick",typekey=833) 

        call prepare(lib_xbrick(834),key=834, & 
& nodecnc=[680,724,648,617,3252,3296,3220,3189,15493, 15494,6568, 6567,14884, 14883,15495, 15496,15497 & 
& , 15498,6576, 6575,14888, 14887,15499, 15500], & 
& edgecnc=[5175,712,4870,5176,5177,716,4872,5178], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(834),elname="xbrick",eltype="xbrick",typekey=834) 

        call prepare(lib_xbrick(835),key=835, & 
& nodecnc=[730,680,617,2265,3302,3252,3189,4837,15501, 15502,15496, 15495,14874, 14873,15503, 15504,15505 & 
& , 15506,15500, 15499,14878, 14877,15507, 15508], & 
& edgecnc=[5179,5176,4865,5180,5181,5178,4867,5182], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(835),elname="xbrick",eltype="xbrick",typekey=835) 

        call prepare(lib_xbrick(836),key=836, & 
& nodecnc=[732,679,650,688,3304,3251,3222,3260,8850, 8849,15020, 15019,15509, 15510,15511, 15512,8858 & 
& , 8857,15024, 15023,15513, 15514,15515, 15516], & 
& edgecnc=[1853,4938,5183,5184,1857,4940,5185,5186], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(836),elname="xbrick",eltype="xbrick",typekey=836) 

        call prepare(lib_xbrick(837),key=837, & 
& nodecnc=[719,683,638,2262,3291,3255,3210,4834,15517, 15518,15519, 15520,15150, 15149,15521, 15522,15523 & 
& , 15524,15525, 15526,15154, 15153,15527, 15528], & 
& edgecnc=[5187,5188,5003,5189,5190,5191,5005,5192], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(837),elname="xbrick",eltype="xbrick",typekey=837) 

        call prepare(lib_xbrick(838),key=838, & 
& nodecnc=[574,638,683,647,3146,3210,3255,3219,14792, 14791,15520, 15519,14768, 14767,15158, 15157,14798 & 
& , 14797,15526, 15525,14774, 14773,15162, 15161], & 
& edgecnc=[4824,5188,4812,5007,4827,5191,4815,5009], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(838),elname="xbrick",eltype="xbrick",typekey=838) 

        call prepare(lib_xbrick(839),key=839, & 
& nodecnc=[624,2264,2308,676,3196,4836,4880,3248,14866, 14865,15529, 15530,15531, 15532,15324, 15323,14870 & 
& , 14869,15533, 15534,15535, 15536,15332, 15331], & 
& edgecnc=[4861,5193,5194,5090,4863,5195,5196,5094], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(839),elname="xbrick",eltype="xbrick",typekey=839) 

        call prepare(lib_xbrick(840),key=840, & 
& nodecnc=[724,680,2365,755,3296,3252,4937,3327,15494, 15493,15537, 15538,15539, 15540,15541, 15542,15498 & 
& , 15497,15543, 15544,15545, 15546,15547, 15548], & 
& edgecnc=[5175,5197,5198,5199,5177,5200,5201,5202], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(840),elname="xbrick",eltype="xbrick",typekey=840) 

        call prepare(lib_xbrick(841),key=841, & 
& nodecnc=[2272,623,678,2315,4844,3195,3250,4887,14594, 14593,15549, 15550,15134, 15133,15372, 15371,14602 & 
& , 14601,15551, 15552,15142, 15141,15376, 15375], & 
& edgecnc=[4725,5203,4995,5114,4729,5204,4999,5116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(841),elname="xbrick",eltype="xbrick",typekey=841) 

        call prepare(lib_xbrick(842),key=842, & 
& nodecnc=[2316,678,623,649,4888,3250,3195,3221,15553, 15554,15550, 15549,14842, 14841,15308, 15307,15555 & 
& , 15556,15552, 15551,14844, 14843,15314, 15313], & 
& edgecnc=[5205,5203,4849,5082,5206,5204,4850,5085], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(842),elname="xbrick",eltype="xbrick",typekey=842) 

        call prepare(lib_xbrick(843),key=843, & 
& nodecnc=[757,2341,719,2262,3329,4913,3291,4834,15557, 15558,15559, 15560,15522, 15521,15561, 15562,15563 & 
& , 15564,15565, 15566,15528, 15527,15567, 15568], & 
& edgecnc=[5207,5208,5189,5209,5210,5211,5192,5212], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(843),elname="xbrick",eltype="xbrick",typekey=843) 

        call prepare(lib_xbrick(844),key=844, & 
& nodecnc=[622,613,652,655,3194,3185,3224,3227,14954, 14953,15192, 15191,15254, 15253,15569, 15570,14960 & 
& , 14959,15200, 15199,15262, 15261,15571, 15572], & 
& edgecnc=[4905,5024,5055,5213,4908,5028,5059,5214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(844),elname="xbrick",eltype="xbrick",typekey=844) 

        call prepare(lib_xbrick(845),key=845, & 
& nodecnc=[2317,694,619,660,4889,3266,3191,3232,15573, 15574,15410, 15409,14514, 14513,15575, 15576,15577 & 
& , 15578,15414, 15413,14522, 14521,15579, 15580], & 
& edgecnc=[5215,5133,4685,5216,5217,5135,4689,5218], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(845),elname="xbrick",eltype="xbrick",typekey=845) 

        call prepare(lib_xbrick(846),key=846, & 
& nodecnc=[711,677,157,684,3283,3249,2729,3256,15581, 15582,15583, 15584,14814, 14813,11204, 11203,15585 & 
& , 15586,15587, 15588,14820, 14819,11212, 11211], & 
& edgecnc=[5219,5220,4835,3030,5221,5222,4838,3034], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(846),elname="xbrick",eltype="xbrick",typekey=846) 

        call prepare(lib_xbrick(847),key=847, & 
& nodecnc=[712,748,684,653,3284,3320,3256,3225,15589, 15590,11206, 11205,14812, 14811,8608, 8607,15591 & 
& , 15592,11214, 11213,14818, 14817,8616, 8615], & 
& edgecnc=[5223,3031,4834,1732,5224,3035,4837,1736], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(847),elname="xbrick",eltype="xbrick",typekey=847) 

        call prepare(lib_xbrick(848),key=848, & 
& nodecnc=[656,721,693,2275,3228,3293,3265,4847,15286, 15285,15593, 15594,15212, 15211,15595, 15596,15294 & 
& , 15293,15597, 15598,15220, 15219,15599, 15600], & 
& edgecnc=[5071,5225,5034,5226,5075,5227,5038,5228], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(848),elname="xbrick",eltype="xbrick",typekey=848) 

        call prepare(lib_xbrick(849),key=849, & 
& nodecnc=[2275,615,621,656,4847,3187,3193,3228,15282, 15281,14940, 14939,15601, 15602,15596, 15595,15284 & 
& , 15283,14946, 14945,15603, 15604,15600, 15599], & 
& edgecnc=[5069,4898,5229,5226,5070,4901,5230,5228], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(849),elname="xbrick",eltype="xbrick",typekey=849) 

        call prepare(lib_xbrick(850),key=850, & 
& nodecnc=[2388,657,654,139,4960,3229,3226,2711,15605, 15606,15228, 15227,15216, 15215,15607, 15608,15609 & 
& , 15610,15234, 15233,15224, 15223,15611, 15612], & 
& edgecnc=[5231,5042,5036,5232,5233,5045,5040,5234], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(850),elname="xbrick",eltype="xbrick",typekey=850) 

        call prepare(lib_xbrick(851),key=851, & 
& nodecnc=[2339,636,616,657,4911,3208,3188,3229,15440, 15439,15446, 15445,15230, 15229,15613, 15614,15444 & 
& , 15443,15448, 15447,15236, 15235,15615, 15616], & 
& edgecnc=[5148,5151,5043,5235,5150,5152,5046,5236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(851),elname="xbrick",eltype="xbrick",typekey=851) 

        call prepare(lib_xbrick(852),key=852, & 
& nodecnc=[2297,695,2274,658,4869,3267,4846,3230,8764, 8763,15290, 15289,14926, 14925,15617, 15618,8772 & 
& , 8771,15298, 15297,14934, 14933,15619, 15620], & 
& edgecnc=[1810,5073,4891,5237,1814,5077,4895,5238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(852),elname="xbrick",eltype="xbrick",typekey=852) 

        call prepare(lib_xbrick(853),key=853, & 
& nodecnc=[656,621,2249,2274,3228,3193,4821,4846,15602, 15601,15270, 15269,14928, 14927,15288, 15287,15604 & 
& , 15603,15272, 15271,14936, 14935,15296, 15295], & 
& edgecnc=[5229,5063,4892,5072,5230,5064,4896,5076], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(853),elname="xbrick",eltype="xbrick",typekey=853) 

        call prepare(lib_xbrick(854),key=854, & 
& nodecnc=[698,659,2311,2368,3270,3231,4883,4940,15621, 15622,15242, 15241,15623, 15624,15625, 15626,15627 & 
& , 15628,15248, 15247,15629, 15630,15631, 15632], & 
& edgecnc=[5239,5049,5240,5241,5242,5052,5243,5244], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(854),elname="xbrick",eltype="xbrick",typekey=854) 

        call prepare(lib_xbrick(855),key=855, & 
& nodecnc=[666,2314,2271,628,3238,4886,4843,3200,15633, 15634,15635, 15636,15010, 15009,15346, 15345,15637 & 
& , 15638,15639, 15640,15014, 15013,15352, 15351], & 
& edgecnc=[5245,5246,4933,5101,5247,5248,4935,5104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(855),elname="xbrick",eltype="xbrick",typekey=855) 

        call prepare(lib_xbrick(856),key=856, & 
& nodecnc=[157,677,681,642,2729,3249,3253,3214,15584, 15583,15641, 15642,15462, 15461,14754, 14753,15588 & 
& , 15587,15643, 15644,15466, 15465,14760, 14759], & 
& edgecnc=[5220,5249,5159,4805,5222,5250,5161,4808], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(856),elname="xbrick",eltype="xbrick",typekey=856) 

        call prepare(lib_xbrick(857),key=857, & 
& nodecnc=[727,701,2320,669,3299,3273,4892,3241,15645, 15646,15647, 15648,15649, 15650,8702, 8701,15651 & 
& , 15652,15653, 15654,15655, 15656,8710, 8709], & 
& edgecnc=[5251,5252,5253,1779,5254,5255,5256,1783], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(857),elname="xbrick",eltype="xbrick",typekey=857) 

        call prepare(lib_xbrick(858),key=858, & 
& nodecnc=[2342,666,664,2343,4914,3238,3236,4915,15657, 15658,15344, 15343,15659, 15660,15661, 15662,15663 & 
& , 15664,15350, 15349,15665, 15666,15667, 15668], & 
& edgecnc=[5257,5100,5258,5259,5260,5103,5261,5262], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(858),elname="xbrick",eltype="xbrick",typekey=858) 

        call prepare(lib_xbrick(859),key=859, & 
& nodecnc=[751,699,665,714,3323,3271,3237,3286,15669, 15670,15404, 15403,15378, 15377,15671, 15672,15673 & 
& , 15674,15408, 15407,15380, 15379,15675, 15676], & 
& edgecnc=[5263,5130,5117,5264,5265,5132,5118,5266], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(859),elname="xbrick",eltype="xbrick",typekey=859) 

        call prepare(lib_xbrick(860),key=860, & 
& nodecnc=[634,2271,703,668,3206,4843,3275,3240,15012, 15011,15677, 15678,15679, 15680,15418, 15417,15016 & 
& , 15015,15681, 15682,15683, 15684,15422, 15421], & 
& edgecnc=[4934,5267,5268,5137,4936,5269,5270,5139], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(860),elname="xbrick",eltype="xbrick",typekey=860) 

        call prepare(lib_xbrick(861),key=861, & 
& nodecnc=[702,703,2271,2314,3274,3275,4843,4886,15685, 15686,15678, 15677,15636, 15635,15687, 15688,15689 & 
& , 15690,15682, 15681,15640, 15639,15691, 15692], & 
& edgecnc=[5271,5267,5246,5272,5273,5269,5248,5274], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(861),elname="xbrick",eltype="xbrick",typekey=861) 

        call prepare(lib_xbrick(862),key=862, & 
& nodecnc=[668,700,2317,660,3240,3272,4889,3232,15693, 15694,6684, 6683,15576, 15575,15420, 15419,15695 & 
& , 15696,6692, 6691,15580, 15579,15424, 15423], & 
& edgecnc=[5275,770,5216,5138,5276,774,5218,5140], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(862),elname="xbrick",eltype="xbrick",typekey=862) 

        call prepare(lib_xbrick(863),key=863, & 
& nodecnc=[739,700,668,703,3311,3272,3240,3275,15697, 15698,15694, 15693,15680, 15679,15699, 15700,15701 & 
& , 15702,15696, 15695,15684, 15683,15703, 15704], & 
& edgecnc=[5277,5275,5268,5278,5279,5276,5270,5280], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(863),elname="xbrick",eltype="xbrick",typekey=863) 

        call prepare(lib_xbrick(864),key=864, & 
& nodecnc=[2313,671,670,705,4885,3243,3242,3277,15705, 15706,15450, 15449,15707, 15708,6622, 6621,15709 & 
& , 15710,15452, 15451,15711, 15712,6630, 6629], & 
& edgecnc=[5281,5153,5282,739,5283,5154,5284,743], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(864),elname="xbrick",eltype="xbrick",typekey=864) 

        call prepare(lib_xbrick(865),key=865, & 
& nodecnc=[190,706,2269,2347,2762,3278,4841,4919,15713, 15714,15100, 15099,15715, 15716,15717, 15718,15719 & 
& , 15720,15108, 15107,15721, 15722,15723, 15724], & 
& edgecnc=[5285,4978,5286,5287,5288,4982,5289,5290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(865),elname="xbrick",eltype="xbrick",typekey=865) 

        call prepare(lib_xbrick(866),key=866, & 
& nodecnc=[2311,667,704,2368,4883,3239,3276,4940,15336, 15335,15725, 15726,15727, 15728,15624, 15623,15340 & 
& , 15339,15729, 15730,15731, 15732,15630, 15629], & 
& edgecnc=[5096,5291,5292,5240,5098,5293,5294,5243], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(866),elname="xbrick",eltype="xbrick",typekey=866) 

        call prepare(lib_xbrick(867),key=867, & 
& nodecnc=[675,614,2247,2266,3247,3186,4819,4838,15733, 15734,14746, 14745,15735, 15736,15356, 15355,15737 & 
& , 15738,14750, 14749,15739, 15740,15364, 15363], & 
& edgecnc=[5295,4801,5296,5106,5297,4803,5298,5110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(867),elname="xbrick",eltype="xbrick",typekey=867) 

        call prepare(lib_xbrick(868),key=868, & 
& nodecnc=[710,708,672,673,3282,3280,3244,3245,15741, 15742,15743, 15744,14848, 14847,15320, 15319,15745 & 
& , 15746,15747, 15748,14854, 14853,15328, 15327], & 
& edgecnc=[5299,5300,4852,5088,5301,5302,4855,5092], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(868),elname="xbrick",eltype="xbrick",typekey=868) 

        call prepare(lib_xbrick(869),key=869, & 
& nodecnc=[686,2321,697,644,3258,4893,3269,3216,15749, 15750,15751, 15752,15753, 15754,15048, 15047,15755 & 
& , 15756,15757, 15758,15759, 15760,15056, 15055], & 
& edgecnc=[5303,5304,5305,4952,5306,5307,5308,4956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(869),elname="xbrick",eltype="xbrick",typekey=869) 

        call prepare(lib_xbrick(870),key=870, & 
& nodecnc=[697,674,611,644,3269,3246,3183,3216,15761, 15762,14784, 14783,15174, 15173,15754, 15753,15763 & 
& , 15764,14788, 14787,15176, 15175,15760, 15759], & 
& edgecnc=[5309,4820,5015,5305,5310,4822,5016,5308], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(870),elname="xbrick",eltype="xbrick",typekey=870) 

        call prepare(lib_xbrick(871),key=871, & 
& nodecnc=[726,689,614,675,3298,3261,3186,3247,15765, 15766,5528, 5527,15734, 15733,15354, 15353,15767 & 
& , 15768,5536, 5535,15738, 15737,15362, 15361], & 
& edgecnc=[5311,192,5295,5105,5312,196,5297,5109], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(871),elname="xbrick",eltype="xbrick",typekey=871) 

        call prepare(lib_xbrick(872),key=872, & 
& nodecnc=[2266,672,708,709,4838,3244,3280,3281,15769, 15770,15744, 15743,15771, 15772,15358, 15357,15773 & 
& , 15774,15748, 15747,15775, 15776,15366, 15365], & 
& edgecnc=[5313,5300,5314,5107,5315,5302,5316,5111], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(872),elname="xbrick",eltype="xbrick",typekey=872) 

        call prepare(lib_xbrick(873),key=873, & 
& nodecnc=[759,710,676,717,3331,3282,3248,3289,15777, 15778,15318, 15317,15779, 15780,15781, 15782,15783 & 
& , 15784,15326, 15325,15785, 15786,15787, 15788], & 
& edgecnc=[5317,5087,5318,5319,5320,5091,5321,5322], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(873),elname="xbrick",eltype="xbrick",typekey=873) 

        call prepare(lib_xbrick(874),key=874, & 
& nodecnc=[2364,687,2308,2309,4936,3259,4880,4881,15789, 15790,15791, 15792,15793, 15794,15795, 15796 & 
& ,15797, 15798,15799, 15800,15801, 15802,15803, 15804], & 
& edgecnc=[5323,5324,5325,5326,5327,5328,5329,5330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(874),elname="xbrick",eltype="xbrick",typekey=874) 

        call prepare(lib_xbrick(875),key=875, & 
& nodecnc=[784,732,688,758,3356,3304,3260,3330,15805, 15806,15512, 15511,15807, 15808,15116, 15115,15809 & 
& , 15810,15516, 15515,15811, 15812,15124, 15123], & 
& edgecnc=[5331,5184,5332,4986,5333,5186,5334,4990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(875),elname="xbrick",eltype="xbrick",typekey=875) 

        call prepare(lib_xbrick(876),key=876, & 
& nodecnc=[715,688,650,607,3287,3260,3222,3179,15813, 15814,15510, 15509,14830, 14829,15815, 15816,15817 & 
& , 15818,15514, 15513,14836, 14835,15819, 15820], & 
& edgecnc=[5335,5183,4843,5336,5337,5185,4846,5338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(876),elname="xbrick",eltype="xbrick",typekey=876) 

        call prepare(lib_xbrick(877),key=877, & 
& nodecnc=[723,2352,756,764,3295,4924,3328,3336,15821, 15822,15823, 15824,15825, 15826,15827, 15828,15829 & 
& , 15830,15831, 15832,15833, 15834,15835, 15836], & 
& edgecnc=[5339,5340,5341,5342,5343,5344,5345,5346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(877),elname="xbrick",eltype="xbrick",typekey=877) 

        call prepare(lib_xbrick(878),key=878, & 
& nodecnc=[723,730,2265,2310,3295,3302,4837,4882,15837, 15838,15504, 15503,12034, 12033,15839, 15840,15841 & 
& , 15842,15508, 15507,12042, 12041,15843, 15844], & 
& edgecnc=[5347,5180,3445,5348,5349,5182,3449,5350], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(878),elname="xbrick",eltype="xbrick",typekey=878) 

        call prepare(lib_xbrick(879),key=879, & 
& nodecnc=[681,716,691,661,3253,3288,3263,3233,15845, 15846,15847, 15848,15486, 15485,15464, 15463,15849 & 
& , 15850,15851, 15852,15490, 15489,15468, 15467], & 
& edgecnc=[5351,5352,5171,5160,5353,5354,5173,5162], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(879),elname="xbrick",eltype="xbrick",typekey=879) 

        call prepare(lib_xbrick(880),key=880, & 
& nodecnc=[725,752,827,2399,3297,3324,3399,4971,15853, 15854,15855, 15856,15857, 15858,15859, 15860,15861 & 
& , 15862,15863, 15864,15865, 15866,15867, 15868], & 
& edgecnc=[5355,5356,5357,5358,5359,5360,5361,5362], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(880),elname="xbrick",eltype="xbrick",typekey=880) 

        call prepare(lib_xbrick(881),key=881, & 
& nodecnc=[718,682,608,646,3290,3254,3180,3218,15869, 15870,15310, 15309,15458, 15457,8332, 8331,15871 & 
& , 15872,15316, 15315,15460, 15459,8340, 8339], & 
& edgecnc=[5363,5083,5157,1594,5364,5086,5158,1598], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(881),elname="xbrick",eltype="xbrick",typekey=881) 

        call prepare(lib_xbrick(882),key=882, & 
& nodecnc=[719,747,692,683,3291,3319,3264,3255,15873, 15874,15875, 15876,14770, 14769,15518, 15517,15877 & 
& , 15878,15879, 15880,14776, 14775,15524, 15523], & 
& edgecnc=[5365,5366,4813,5187,5367,5368,4816,5190], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(882),elname="xbrick",eltype="xbrick",typekey=882) 

        call prepare(lib_xbrick(883),key=883, & 
& nodecnc=[685,720,760,765,3257,3292,3332,3337,15881, 15882,15883, 15884,15885, 15886,8612, 8611,15887 & 
& , 15888,15889, 15890,15891, 15892,8620, 8619], & 
& edgecnc=[5369,5370,5371,1734,5372,5373,5374,1738], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(883),elname="xbrick",eltype="xbrick",typekey=883) 

        call prepare(lib_xbrick(884),key=884, & 
& nodecnc=[720,685,639,674,3292,3257,3211,3246,15882, 15881,15168, 15167,14782, 14781,15893, 15894,15888 & 
& , 15887,15172, 15171,14786, 14785,15895, 15896], & 
& edgecnc=[5369,5012,4819,5375,5372,5014,4821,5376], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(884),elname="xbrick",eltype="xbrick",typekey=884) 

        call prepare(lib_xbrick(885),key=885, & 
& nodecnc=[659,2310,2267,618,3231,4882,4839,3190,15897, 15898,12032, 12031,15238, 15237,15244, 15243,15899 & 
& , 15900,12040, 12039,15240, 15239,15250, 15249], & 
& edgecnc=[5377,3444,5047,5050,5378,3448,5048,5053], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(885),elname="xbrick",eltype="xbrick",typekey=885) 

        call prepare(lib_xbrick(886),key=886, & 
& nodecnc=[778,795,717,687,3350,3367,3289,3259,15901, 15902,15903, 15904,15905, 15906,15907, 15908,15909 & 
& , 15910,15911, 15912,15913, 15914,15915, 15916], & 
& edgecnc=[5379,5380,5381,5382,5383,5384,5385,5386], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(886),elname="xbrick",eltype="xbrick",typekey=886) 

        call prepare(lib_xbrick(887),key=887, & 
& nodecnc=[607,645,750,715,3179,3217,3322,3287,15130, 15129,15917, 15918,15919, 15920,15816, 15815,15132 & 
& , 15131,15921, 15922,15923, 15924,15820, 15819], & 
& edgecnc=[4993,5387,5388,5336,4994,5389,5390,5338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(887),elname="xbrick",eltype="xbrick",typekey=887) 

        call prepare(lib_xbrick(888),key=888, & 
& nodecnc=[791,750,645,689,3363,3322,3217,3261,15925, 15926,15918, 15917,5522, 5521,15927, 15928,15929 & 
& , 15930,15922, 15921,5530, 5529,15931, 15932], & 
& edgecnc=[5391,5387,189,5392,5393,5389,193,5394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(888),elname="xbrick",eltype="xbrick",typekey=888) 

        call prepare(lib_xbrick(889),key=889, & 
& nodecnc=[2407,757,2340,690,4979,3329,4912,3262,15933, 15934,15935, 15936,15937, 15938,15470, 15469,15939 & 
& , 15940,15941, 15942,15943, 15944,15478, 15477], & 
& edgecnc=[5395,5396,5397,5163,5398,5399,5400,5167], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(889),elname="xbrick",eltype="xbrick",typekey=889) 

        call prepare(lib_xbrick(890),key=890, & 
& nodecnc=[2340,651,641,690,4912,3223,3213,3262,15945, 15946,14644, 14643,15488, 15487,15938, 15937,15947 & 
& , 15948,14652, 14651,15492, 15491,15944, 15943], & 
& edgecnc=[5401,4750,5172,5397,5402,4754,5174,5400], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(890),elname="xbrick",eltype="xbrick",typekey=890) 

        call prepare(lib_xbrick(891),key=891, & 
& nodecnc=[677,767,716,681,3249,3339,3288,3253,15949, 15950,15951, 15952,15846, 15845,15642, 15641,15953 & 
& , 15954,15955, 15956,15850, 15849,15644, 15643], & 
& edgecnc=[5403,5404,5351,5249,5405,5406,5353,5250], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(891),elname="xbrick",eltype="xbrick",typekey=891) 

        call prepare(lib_xbrick(892),key=892, & 
& nodecnc=[2388,139,734,729,4960,2711,3306,3301,15608, 15607,15957, 15958,15959, 15960,15961, 15962,15612 & 
& , 15611,15963, 15964,15965, 15966,15967, 15968], & 
& edgecnc=[5232,5407,5408,5409,5234,5410,5411,5412], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(892),elname="xbrick",eltype="xbrick",typekey=892) 

        call prepare(lib_xbrick(893),key=893, & 
& nodecnc=[190,2347,731,2367,2762,4919,3303,4939,15718, 15717,15258, 15257,15969, 15970,15971, 15972,15724 & 
& , 15723,15266, 15265,15973, 15974,15975, 15976], & 
& edgecnc=[5287,5057,5413,5414,5290,5061,5415,5416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(893),elname="xbrick",eltype="xbrick",typekey=893) 

        call prepare(lib_xbrick(894),key=894, & 
& nodecnc=[694,2317,744,2349,3266,4889,3316,4921,15574, 15573,6682, 6681,15977, 15978,15412, 15411,15578 & 
& , 15577,6690, 6689,15979, 15980,15416, 15415], & 
& edgecnc=[5215,769,5417,5134,5217,773,5418,5136], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(894),elname="xbrick",eltype="xbrick",typekey=894) 

        call prepare(lib_xbrick(895),key=895, & 
& nodecnc=[737,770,735,2297,3309,3342,3307,4869,15981, 15982,15983, 15984,8758, 8757,15985, 15986,15987 & 
& , 15988,15989, 15990,8766, 8765,15991, 15992], & 
& edgecnc=[5419,5420,1807,5421,5422,5423,1811,5424], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(895),elname="xbrick",eltype="xbrick",typekey=895) 

        call prepare(lib_xbrick(896),key=896, & 
& nodecnc=[663,699,737,696,3235,3271,3309,3268,15402, 15401,15993, 15994,15995, 15996,15396, 15395,15406 & 
& , 15405,15997, 15998,15999, 16000,15400, 15399], & 
& edgecnc=[5129,5425,5426,5126,5131,5427,5428,5128], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(896),elname="xbrick",eltype="xbrick",typekey=896) 

        call prepare(lib_xbrick(897),key=897, & 
& nodecnc=[720,2273,788,760,3292,4845,3360,3332,16001, 16002,16003, 16004,16005, 16006,15884, 15883,16007 & 
& , 16008,16009, 16010,16011, 16012,15890, 15889], & 
& edgecnc=[5429,5430,5431,5370,5432,5433,5434,5373], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(897),elname="xbrick",eltype="xbrick",typekey=897) 

        call prepare(lib_xbrick(898),key=898, & 
& nodecnc=[659,2352,723,2310,3231,4924,3295,4882,16013, 16014,15822, 15821,15840, 15839,15898, 15897,16015 & 
& , 16016,15830, 15829,15844, 15843,15900, 15899], & 
& edgecnc=[5435,5339,5348,5377,5436,5343,5350,5378], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(898),elname="xbrick",eltype="xbrick",typekey=898) 

        call prepare(lib_xbrick(899),key=899, & 
& nodecnc=[2369,698,2401,738,4941,3270,4973,3310,16017, 16018,16019, 16020,16021, 16022,16023, 16024,16025 & 
& , 16026,16027, 16028,16029, 16030,16031, 16032], & 
& edgecnc=[5437,5438,5439,5440,5441,5442,5443,5444], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(899),elname="xbrick",eltype="xbrick",typekey=899) 

        call prepare(lib_xbrick(900),key=900, & 
& nodecnc=[140,737,699,751,2712,3309,3271,3323,16033, 16034,15994, 15993,15670, 15669,11224, 11223,16035 & 
& , 16036,15998, 15997,15674, 15673,11232, 11231], & 
& edgecnc=[5445,5425,5263,3040,5446,5427,5265,3044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(900),elname="xbrick",eltype="xbrick",typekey=900) 

        call prepare(lib_xbrick(901),key=901, & 
& nodecnc=[666,2342,702,2314,3238,4914,3274,4886,15658, 15657,16037, 16038,15688, 15687,15634, 15633,15664 & 
& , 15663,16039, 16040,15692, 15691,15638, 15637], & 
& edgecnc=[5257,5447,5272,5245,5260,5448,5274,5247], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(901),elname="xbrick",eltype="xbrick",typekey=901) 

        call prepare(lib_xbrick(902),key=902, & 
& nodecnc=[785,809,701,746,3357,3381,3273,3318,16041, 16042,16043, 16044,16045, 16046,12748, 12747,16047 & 
& , 16048,16049, 16050,16051, 16052,12756, 12755], & 
& edgecnc=[5449,5450,5451,3802,5452,5453,5454,3806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(902),elname="xbrick",eltype="xbrick",typekey=902) 

        call prepare(lib_xbrick(903),key=903, & 
& nodecnc=[2373,740,702,753,4945,3312,3274,3325,16053, 16054,16055, 16056,16057, 16058,16059, 16060,16061 & 
& , 16062,16063, 16064,16065, 16066,16067, 16068], & 
& edgecnc=[5455,5456,5457,5458,5459,5460,5461,5462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(903),elname="xbrick",eltype="xbrick",typekey=903) 

        call prepare(lib_xbrick(904),key=904, & 
& nodecnc=[702,740,739,703,3274,3312,3311,3275,16056, 16055,16069, 16070,15700, 15699,15686, 15685,16064 & 
& , 16063,16071, 16072,15704, 15703,15690, 15689], & 
& edgecnc=[5456,5463,5278,5271,5460,5464,5280,5273], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(904),elname="xbrick",eltype="xbrick",typekey=904) 

        call prepare(lib_xbrick(905),key=905, & 
& nodecnc=[2401,704,2370,741,4973,3276,4942,3313,16073, 16074,16075, 16076,16077, 16078,16079, 16080,16081 & 
& , 16082,16083, 16084,16085, 16086,16087, 16088], & 
& edgecnc=[5465,5466,5467,5468,5469,5470,5471,5472], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(905),elname="xbrick",eltype="xbrick",typekey=905) 

        call prepare(lib_xbrick(906),key=906, & 
& nodecnc=[2312,2370,704,667,4884,4942,3276,3239,16089, 16090,16076, 16075,15726, 15725,15430, 15429,16091 & 
& , 16092,16084, 16083,15730, 15729,15436, 15435], & 
& edgecnc=[5473,5466,5291,5143,5474,5470,5293,5146], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(906),elname="xbrick",eltype="xbrick",typekey=906) 

        call prepare(lib_xbrick(907),key=907, & 
& nodecnc=[776,2405,775,2371,3348,4977,3347,4943,16093, 16094,16095, 16096,16097, 16098,6642, 6641,16099 & 
& , 16100,16101, 16102,16103, 16104,6648, 6647], & 
& edgecnc=[5475,5476,5477,749,5478,5479,5480,752], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(907),elname="xbrick",eltype="xbrick",typekey=907) 

        call prepare(lib_xbrick(908),key=908, & 
& nodecnc=[745,705,670,706,3317,3277,3242,3278,6638, 6637,15708, 15707,15102, 15101,16105, 16106,6644 & 
& , 6643,15712, 15711,15110, 15109,16107, 16108], & 
& edgecnc=[747,5282,4979,5481,750,5284,4983,5482], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(908),elname="xbrick",eltype="xbrick",typekey=908) 

        call prepare(lib_xbrick(909),key=909, & 
& nodecnc=[2432,771,727,707,5004,3343,3299,3279,16109, 16110,16111, 16112,8700, 8699,16113, 16114,16115 & 
& , 16116,16117, 16118,8708, 8707,16119, 16120], & 
& edgecnc=[5483,5484,1778,5485,5486,5487,1782,5488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(909),elname="xbrick",eltype="xbrick",typekey=909) 

        call prepare(lib_xbrick(910),key=910, & 
& nodecnc=[729,2432,707,2388,3301,5004,3279,4960,16121, 16122,16114, 16113,16123, 16124,15962, 15961,16125 & 
& , 16126,16120, 16119,16127, 16128,15968, 15967], & 
& edgecnc=[5489,5485,5490,5409,5491,5488,5492,5412], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(910),elname="xbrick",eltype="xbrick",typekey=910) 

        call prepare(lib_xbrick(911),key=911, & 
& nodecnc=[762,742,779,794,3334,3314,3351,3366,16129, 16130,16131, 16132,16133, 16134,16135, 16136,16137 & 
& , 16138,16139, 16140,16141, 16142,16143, 16144], & 
& edgecnc=[5493,5494,5495,5496,5497,5498,5499,5500], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(911),elname="xbrick",eltype="xbrick",typekey=911) 

        call prepare(lib_xbrick(912),key=912, & 
& nodecnc=[709,708,743,742,3281,3280,3315,3314,15772, 15771,16145, 16146,16147, 16148,16149, 16150,15776 & 
& , 15775,16151, 16152,16153, 16154,16155, 16156], & 
& edgecnc=[5314,5501,5502,5503,5316,5504,5505,5506], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(912),elname="xbrick",eltype="xbrick",typekey=912) 

        call prepare(lib_xbrick(913),key=913, & 
& nodecnc=[742,762,726,709,3314,3334,3298,3281,16130, 16129,16157, 16158,15360, 15359,16150, 16149,16138 & 
& , 16137,16159, 16160,15368, 15367,16156, 16155], & 
& edgecnc=[5493,5507,5108,5503,5497,5508,5112,5506], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(913),elname="xbrick",eltype="xbrick",typekey=913) 

        call prepare(lib_xbrick(914),key=914, & 
& nodecnc=[708,710,759,743,3280,3282,3331,3315,15742, 15741,15778, 15777,16161, 16162,16146, 16145,15746 & 
& , 15745,15784, 15783,16163, 16164,16152, 16151], & 
& edgecnc=[5299,5317,5509,5501,5301,5320,5510,5504], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(914),elname="xbrick",eltype="xbrick",typekey=914) 

        call prepare(lib_xbrick(915),key=915, & 
& nodecnc=[824,862,799,2306,3396,3434,3371,4878,16165, 16166,16167, 16168,16169, 16170,16171, 16172,16173 & 
& , 16174,16175, 16176,16177, 16178,16179, 16180], & 
& edgecnc=[5511,5512,5513,5514,5515,5516,5517,5518], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(915),elname="xbrick",eltype="xbrick",typekey=915) 

        call prepare(lib_xbrick(916),key=916, & 
& nodecnc=[802,748,712,765,3374,3320,3284,3337,16181, 16182,15590, 15589,8606, 8605,16183, 16184,16185 & 
& , 16186,15592, 15591,8614, 8613,16187, 16188], & 
& edgecnc=[5519,5223,1731,5520,5521,5224,1735,5522], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(916),elname="xbrick",eltype="xbrick",typekey=916) 

        call prepare(lib_xbrick(917),key=917, & 
& nodecnc=[800,2425,2373,753,3372,4997,4945,3325,12068, 12067,16189, 16190,16060, 16059,16191, 16192,12076 & 
& , 12075,16193, 16194,16068, 16067,16195, 16196], & 
& edgecnc=[3462,5523,5458,5524,3466,5525,5462,5526], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(917),elname="xbrick",eltype="xbrick",typekey=917) 

        call prepare(lib_xbrick(918),key=918, & 
& nodecnc=[2316,682,752,725,4888,3254,3324,3297,15306, 15305,16197, 16198,15854, 15853,16199, 16200,15312 & 
& , 15311,16201, 16202,15862, 15861,16203, 16204], & 
& edgecnc=[5081,5527,5355,5528,5084,5529,5359,5530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(918),elname="xbrick",eltype="xbrick",typekey=918) 

        call prepare(lib_xbrick(919),key=919, & 
& nodecnc=[716,749,733,691,3288,3321,3305,3263,16205, 16206,16207, 16208,15474, 15473,15848, 15847,16209 & 
& , 16210,16211, 16212,15482, 15481,15852, 15851], & 
& edgecnc=[5531,5532,5165,5352,5533,5534,5169,5354], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(919),elname="xbrick",eltype="xbrick",typekey=919) 

        call prepare(lib_xbrick(920),key=920, & 
& nodecnc=[749,716,767,780,3321,3288,3339,3352,16206, 16205,15952, 15951,16213, 16214,16215, 16216,16210 & 
& , 16209,15956, 15955,16217, 16218,16219, 16220], & 
& edgecnc=[5531,5404,5535,5536,5533,5406,5537,5538], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(920),elname="xbrick",eltype="xbrick",typekey=920) 

        call prepare(lib_xbrick(921),key=921, & 
& nodecnc=[795,2406,759,717,3367,4978,3331,3289,16221, 16222,16223, 16224,15782, 15781,15904, 15903,16225 & 
& , 16226,16227, 16228,15788, 15787,15912, 15911], & 
& edgecnc=[5539,5540,5319,5380,5541,5542,5322,5384], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(921),elname="xbrick",eltype="xbrick",typekey=921) 

        call prepare(lib_xbrick(922),key=922, & 
& nodecnc=[676,2308,687,717,3248,4880,3259,3289,15532, 15531,15792, 15791,15906, 15905,15780, 15779,15536 & 
& , 15535,15800, 15799,15914, 15913,15786, 15785], & 
& edgecnc=[5194,5324,5381,5318,5196,5328,5385,5321], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(922),elname="xbrick",eltype="xbrick",typekey=922) 

        call prepare(lib_xbrick(923),key=923, & 
& nodecnc=[2338,754,692,747,4910,3326,3264,3319,16229, 16230,8336, 8335,15876, 15875,16231, 16232,16233 & 
& , 16234,8344, 8343,15880, 15879,16235, 16236], & 
& edgecnc=[5543,1596,5366,5544,5545,1600,5368,5546], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(923),elname="xbrick",eltype="xbrick",typekey=923) 

        call prepare(lib_xbrick(924),key=924, & 
& nodecnc=[651,2340,757,2262,3223,4912,3329,4834,15946, 15945,15936, 15935,15562, 15561,15152, 15151,15948 & 
& , 15947,15942, 15941,15568, 15567,15156, 15155], & 
& edgecnc=[5401,5396,5209,5004,5402,5399,5212,5006], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(924),elname="xbrick",eltype="xbrick",typekey=924) 

        call prepare(lib_xbrick(925),key=925, & 
& nodecnc=[697,2321,736,2273,3269,4893,3308,4845,15752, 15751,16237, 16238,16239, 16240,16241, 16242,15758 & 
& , 15757,16243, 16244,16245, 16246,16247, 16248], & 
& edgecnc=[5304,5547,5548,5549,5307,5550,5551,5552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(925),elname="xbrick",eltype="xbrick",typekey=925) 

        call prepare(lib_xbrick(926),key=926, & 
& nodecnc=[2273,720,674,697,4845,3292,3246,3269,16002, 16001,15894, 15893,15762, 15761,16242, 16241,16008 & 
& , 16007,15896, 15895,15764, 15763,16248, 16247], & 
& edgecnc=[5429,5375,5309,5549,5432,5376,5310,5552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(926),elname="xbrick",eltype="xbrick",typekey=926) 

        call prepare(lib_xbrick(927),key=927, & 
& nodecnc=[783,2515,2433,772,3355,5087,5005,3344,16249, 16250,16251, 16252,16253, 16254,16255, 16256,16257 & 
& , 16258,16259, 16260,16261, 16262,16263, 16264], & 
& edgecnc=[5553,5554,5555,5556,5557,5558,5559,5560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(927),elname="xbrick",eltype="xbrick",typekey=927) 

        call prepare(lib_xbrick(928),key=928, & 
& nodecnc=[772,721,695,761,3344,3293,3267,3333,16265, 16266,15292, 15291,8762, 8761,8778, 8777,16267, 16268 & 
& ,15300, 15299,8770, 8769,8784, 8783], & 
& edgecnc=[5561,5074,1809,1817,5562,5078,1813,1820], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(928),elname="xbrick",eltype="xbrick",typekey=928) 

        call prepare(lib_xbrick(929),key=929, & 
& nodecnc=[736,722,809,769,3308,3294,3381,3341,16269, 16270,16271, 16272,16273, 16274,16275, 16276,16277 & 
& , 16278,16279, 16280,16281, 16282,16283, 16284], & 
& edgecnc=[5563,5564,5565,5566,5567,5568,5569,5570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(929),elname="xbrick",eltype="xbrick",typekey=929) 

        call prepare(lib_xbrick(930),key=930, & 
& nodecnc=[207,756,2352,2369,2779,3328,4924,4941,16285, 16286,15824, 15823,16287, 16288,16289, 16290,16291 & 
& , 16292,15832, 15831,16293, 16294,16295, 16296], & 
& edgecnc=[5571,5340,5572,5573,5574,5344,5575,5576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(930),elname="xbrick",eltype="xbrick",typekey=930) 

        call prepare(lib_xbrick(931),key=931, & 
& nodecnc=[2366,790,808,2365,4938,3362,3380,4937,16297, 16298,16299, 16300,16301, 16302,16303, 16304,16305 & 
& , 16306,16307, 16308,16309, 16310,16311, 16312], & 
& edgecnc=[5577,5578,5579,5580,5581,5582,5583,5584], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(931),elname="xbrick",eltype="xbrick",typekey=931) 

        call prepare(lib_xbrick(932),key=932, & 
& nodecnc=[2342,2343,763,2374,4914,4915,3335,4946,15662, 15661,16313, 16314,16315, 16316,16317, 16318 & 
& ,15668, 15667,16319, 16320,16321, 16322,16323, 16324], & 
& edgecnc=[5259,5585,5586,5587,5262,5588,5589,5590], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(932),elname="xbrick",eltype="xbrick",typekey=932) 

        call prepare(lib_xbrick(933),key=933, & 
& nodecnc=[781,2346,725,2399,3353,4918,3297,4971,16325, 16326,16327, 16328,15860, 15859,16329, 16330,16331 & 
& , 16332,16333, 16334,15868, 15867,16335, 16336], & 
& edgecnc=[5591,5592,5358,5593,5594,5595,5362,5596], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(933),elname="xbrick",eltype="xbrick",typekey=933) 

        call prepare(lib_xbrick(934),key=934, & 
& nodecnc=[746,701,727,771,3318,3273,3299,3343,16046, 16045,15646, 15645,16112, 16111,12750, 12749,16052 & 
& , 16051,15652, 15651,16118, 16117,12758, 12757], & 
& edgecnc=[5451,5251,5484,3803,5454,5254,5487,3807], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(934),elname="xbrick",eltype="xbrick",typekey=934) 

        call prepare(lib_xbrick(935),key=935, & 
& nodecnc=[811,810,768,728,3383,3382,3340,3300,16337, 16338,16339, 16340,16341, 16342,16343, 16344,16345 & 
& , 16346,16347, 16348,16349, 16350,16351, 16352], & 
& edgecnc=[5597,5598,5599,5600,5601,5602,5603,5604], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(935),elname="xbrick",eltype="xbrick",typekey=935) 

        call prepare(lib_xbrick(936),key=936, & 
& nodecnc=[2367,728,706,190,4939,3300,3278,2762,16353, 16354,16355, 16356,15714, 15713,15972, 15971,16357 & 
& , 16358,16359, 16360,15720, 15719,15976, 15975], & 
& edgecnc=[5605,5606,5285,5414,5607,5608,5288,5416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(936),elname="xbrick",eltype="xbrick",typekey=936) 

        call prepare(lib_xbrick(937),key=937, & 
& nodecnc=[2475,777,729,734,5047,3349,3301,3306,16361, 16362,16363, 16364,15960, 15959,16365, 16366,16367 & 
& , 16368,16369, 16370,15966, 15965,16371, 16372], & 
& edgecnc=[5609,5610,5408,5611,5612,5613,5411,5614], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(937),elname="xbrick",eltype="xbrick",typekey=937) 

        call prepare(lib_xbrick(938),key=938, & 
& nodecnc=[832,764,756,803,3404,3336,3328,3375,16373, 16374,15826, 15825,16375, 16376,16377, 16378,16379 & 
& , 16380,15834, 15833,16381, 16382,16383, 16384], & 
& edgecnc=[5615,5341,5616,5617,5618,5345,5619,5620], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(938),elname="xbrick",eltype="xbrick",typekey=938) 

        call prepare(lib_xbrick(939),key=939, & 
& nodecnc=[744,731,652,2349,3316,3303,3224,4921,16385, 16386,15256, 15255,15190, 15189,15978, 15977,16387 & 
& , 16388,15264, 15263,15198, 15197,15980, 15979], & 
& edgecnc=[5621,5056,5023,5417,5622,5060,5027,5418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(939),elname="xbrick",eltype="xbrick",typekey=939) 

        call prepare(lib_xbrick(940),key=940, & 
& nodecnc=[801,751,714,766,3373,3323,3286,3338,11218, 11217,15672, 15671,8846, 8845,16389, 16390,11226 & 
& , 11225,15676, 15675,8854, 8853,16391, 16392], & 
& edgecnc=[3037,5264,1851,5623,3041,5266,1855,5624], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(940),elname="xbrick",eltype="xbrick",typekey=940) 

        call prepare(lib_xbrick(941),key=941, & 
& nodecnc=[688,715,807,758,3260,3287,3379,3330,15814, 15813,16393, 16394,15118, 15117,15808, 15807,15818 & 
& , 15817,16395, 16396,15126, 15125,15812, 15811], & 
& edgecnc=[5335,5625,4987,5332,5337,5626,4991,5334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(941),elname="xbrick",eltype="xbrick",typekey=941) 

        call prepare(lib_xbrick(942),key=942, & 
& nodecnc=[749,789,787,733,3321,3361,3359,3305,16397, 16398,16399, 16400,16401, 16402,16208, 16207,16403 & 
& , 16404,16405, 16406,16407, 16408,16212, 16211], & 
& edgecnc=[5627,5628,5629,5532,5630,5631,5632,5534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(942),elname="xbrick",eltype="xbrick",typekey=942) 

        call prepare(lib_xbrick(943),key=943, & 
& nodecnc=[693,721,772,2433,3265,3293,3344,5005,15594, 15593,16266, 16265,16254, 16253,16409, 16410,15598 & 
& , 15597,16268, 16267,16262, 16261,16411, 16412], & 
& edgecnc=[5225,5561,5555,5633,5227,5562,5559,5634], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(943),elname="xbrick",eltype="xbrick",typekey=943) 

        call prepare(lib_xbrick(944),key=944, & 
& nodecnc=[2433,734,139,693,5005,3306,2711,3265,16413, 16414,15958, 15957,15214, 15213,16410, 16409,16415 & 
& , 16416,15964, 15963,15222, 15221,16412, 16411], & 
& edgecnc=[5635,5407,5035,5633,5636,5410,5039,5634], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(944),elname="xbrick",eltype="xbrick",typekey=944) 

        call prepare(lib_xbrick(945),key=945, & 
& nodecnc=[2273,736,769,788,4845,3308,3341,3360,16240, 16239,16276, 16275,16417, 16418,16004, 16003,16246 & 
& , 16245,16284, 16283,16419, 16420,16010, 16009], & 
& edgecnc=[5548,5566,5637,5430,5551,5570,5638,5433], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(945),elname="xbrick",eltype="xbrick",typekey=945) 

        call prepare(lib_xbrick(946),key=946, & 
& nodecnc=[2321,2389,722,736,4893,4961,3294,3308,16421, 16422,16423, 16424,16270, 16269,16238, 16237,16425 & 
& , 16426,16427, 16428,16278, 16277,16244, 16243], & 
& edgecnc=[5639,5640,5563,5547,5641,5642,5567,5550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(946),elname="xbrick",eltype="xbrick",typekey=946) 

        call prepare(lib_xbrick(947),key=947, & 
& nodecnc=[658,696,737,2297,3230,3268,3309,4869,15394, 15393,15996, 15995,15986, 15985,15618, 15617,15398 & 
& , 15397,16000, 15999,15992, 15991,15620, 15619], & 
& edgecnc=[5125,5426,5421,5237,5127,5428,5424,5238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(947),elname="xbrick",eltype="xbrick",typekey=947) 

        call prepare(lib_xbrick(948),key=948, & 
& nodecnc=[2369,738,2430,207,4941,3310,5002,2779,16024, 16023,16429, 16430,16431, 16432,16290, 16289,16032 & 
& , 16031,16433, 16434,16435, 16436,16296, 16295], & 
& edgecnc=[5440,5643,5644,5573,5444,5645,5646,5576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(948),elname="xbrick",eltype="xbrick",typekey=948) 

        call prepare(lib_xbrick(949),key=949, & 
& nodecnc=[2424,774,739,740,4996,3346,3311,3312,16437, 16438,16439, 16440,16070, 16069,16441, 16442,16443 & 
& , 16444,16445, 16446,16072, 16071,16447, 16448], & 
& edgecnc=[5647,5648,5463,5649,5650,5651,5464,5652], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(949),elname="xbrick",eltype="xbrick",typekey=949) 

        call prepare(lib_xbrick(950),key=950, & 
& nodecnc=[2405,738,2401,775,4977,3310,4973,3347,16449, 16450,16022, 16021,16451, 16452,16096, 16095,16453 & 
& , 16454,16030, 16029,16455, 16456,16102, 16101], & 
& edgecnc=[5653,5439,5654,5476,5655,5443,5656,5479], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(950),elname="xbrick",eltype="xbrick",typekey=950) 

        call prepare(lib_xbrick(951),key=951, & 
& nodecnc=[2394,779,742,743,4966,3351,3314,3315,6428, 6427,16132, 16131,16148, 16147,16457, 16458,6436 & 
& , 6435,16140, 16139,16154, 16153,16459, 16460], & 
& edgecnc=[642,5494,5502,5657,646,5498,5505,5658], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(951),elname="xbrick",eltype="xbrick",typekey=951) 

        call prepare(lib_xbrick(952),key=952, & 
& nodecnc=[854,797,2394,2406,3426,3369,4966,4978,16461, 16462,6422, 6421,16463, 16464,16465, 16466,16467 & 
& , 16468,6430, 6429,16469, 16470,16471, 16472], & 
& edgecnc=[5659,639,5660,5661,5662,643,5663,5664], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(952),elname="xbrick",eltype="xbrick",typekey=952) 

        call prepare(lib_xbrick(953),key=953, & 
& nodecnc=[2367,731,744,2431,4939,3303,3316,5003,15970, 15969,16386, 16385,16473, 16474,16475, 16476,15974 & 
& , 15973,16388, 16387,16477, 16478,16479, 16480], & 
& edgecnc=[5413,5621,5665,5666,5415,5622,5667,5668], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(953),elname="xbrick",eltype="xbrick",typekey=953) 

        call prepare(lib_xbrick(954),key=954, & 
& nodecnc=[2350,773,2431,744,4922,3345,5003,3316,16481, 16482,16483, 16484,16474, 16473,6688, 6687,16485 & 
& , 16486,16487, 16488,16478, 16477,6696, 6695], & 
& edgecnc=[5669,5670,5665,772,5671,5672,5667,776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(954),elname="xbrick",eltype="xbrick",typekey=954) 

        call prepare(lib_xbrick(955),key=955, & 
& nodecnc=[806,768,810,840,3378,3340,3382,3412,16489, 16490,16340, 16339,16491, 16492,16493, 16494,16495 & 
& , 16496,16348, 16347,16497, 16498,16499, 16500], & 
& edgecnc=[5673,5598,5674,5675,5676,5602,5677,5678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(955),elname="xbrick",eltype="xbrick",typekey=955) 

        call prepare(lib_xbrick(956),key=956, & 
& nodecnc=[768,745,706,728,3340,3317,3278,3300,16501, 16502,16106, 16105,16356, 16355,16342, 16341,16503 & 
& , 16504,16108, 16107,16360, 16359,16350, 16349], & 
& edgecnc=[5679,5481,5606,5599,5680,5482,5608,5603], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(956),elname="xbrick",eltype="xbrick",typekey=956) 

        call prepare(lib_xbrick(957),key=957, & 
& nodecnc=[2480,819,809,785,5052,3391,3381,3357,16505, 16506,16507, 16508,16042, 16041,16509, 16510,16511 & 
& , 16512,16513, 16514,16048, 16047,16515, 16516], & 
& edgecnc=[5681,5682,5449,5683,5684,5685,5452,5686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(957),elname="xbrick",eltype="xbrick",typekey=957) 

        call prepare(lib_xbrick(958),key=958, & 
& nodecnc=[2507,876,844,796,5079,3448,3416,3368,16517, 16518,16519, 16520,16521, 16522,16523, 16524,16525 & 
& , 16526,16527, 16528,16529, 16530,16531, 16532], & 
& edgecnc=[5687,5688,5689,5690,5691,5692,5693,5694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(958),elname="xbrick",eltype="xbrick",typekey=958) 

        call prepare(lib_xbrick(959),key=959, & 
& nodecnc=[677,711,829,767,3249,3283,3401,3339,15582, 15581,16533, 16534,16535, 16536,15950, 15949,15586 & 
& , 15585,16537, 16538,16539, 16540,15954, 15953], & 
& edgecnc=[5219,5695,5696,5403,5221,5697,5698,5405], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(959),elname="xbrick",eltype="xbrick",typekey=959) 

        call prepare(lib_xbrick(960),key=960, & 
& nodecnc=[839,802,765,799,3411,3374,3337,3371,16541, 16542,16184, 16183,16543, 16544,16545, 16546,16547 & 
& , 16548,16188, 16187,16549, 16550,16551, 16552], & 
& edgecnc=[5699,5520,5700,5701,5702,5522,5703,5704], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(960),elname="xbrick",eltype="xbrick",typekey=960) 

        call prepare(lib_xbrick(961),key=961, & 
& nodecnc=[868,780,767,829,3440,3352,3339,3401,16553, 16554,16214, 16213,16536, 16535,16555, 16556,16557 & 
& , 16558,16218, 16217,16540, 16539,16559, 16560], & 
& edgecnc=[5705,5535,5696,5706,5707,5537,5698,5708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(961),elname="xbrick",eltype="xbrick",typekey=961) 

        call prepare(lib_xbrick(962),key=962, & 
& nodecnc=[689,726,762,791,3261,3298,3334,3363,15766, 15765,16158, 16157,16561, 16562,15928, 15927,15768 & 
& , 15767,16160, 16159,16563, 16564,15932, 15931], & 
& edgecnc=[5311,5507,5709,5392,5312,5508,5710,5394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(962),elname="xbrick",eltype="xbrick",typekey=962) 

        call prepare(lib_xbrick(963),key=963, & 
& nodecnc=[815,2416,781,2469,3387,4988,3353,5041,16565, 16566,16567, 16568,16569, 16570,16571, 16572,16573 & 
& , 16574,16575, 16576,16577, 16578,16579, 16580], & 
& edgecnc=[5711,5712,5713,5714,5715,5716,5717,5718], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(963),elname="xbrick",eltype="xbrick",typekey=963) 

        call prepare(lib_xbrick(964),key=964, & 
& nodecnc=[786,752,682,718,3358,3324,3254,3290,16581, 16582,16198, 16197,15870, 15869,16583, 16584,16585 & 
& , 16586,16202, 16201,15872, 15871,16587, 16588], & 
& edgecnc=[5719,5527,5363,5720,5721,5529,5364,5722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(964),elname="xbrick",eltype="xbrick",typekey=964) 

        call prepare(lib_xbrick(965),key=965, & 
& nodecnc=[702,2342,2374,753,3274,4914,4946,3325,16038, 16037,16318, 16317,16589, 16590,16058, 16057,16040 & 
& , 16039,16324, 16323,16591, 16592,16066, 16065], & 
& edgecnc=[5447,5587,5723,5457,5448,5590,5724,5461], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(965),elname="xbrick",eltype="xbrick",typekey=965) 

        call prepare(lib_xbrick(966),key=966, & 
& nodecnc=[815,800,753,2416,3387,3372,3325,4988,16593, 16594,16192, 16191,16595, 16596,16566, 16565,16597 & 
& , 16598,16196, 16195,16599, 16600,16574, 16573], & 
& edgecnc=[5725,5524,5726,5711,5727,5526,5728,5715], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(966),elname="xbrick",eltype="xbrick",typekey=966) 

        call prepare(lib_xbrick(967),key=967, & 
& nodecnc=[687,2364,2420,778,3259,4936,4992,3350,15790, 15789,16601, 16602,16603, 16604,15908, 15907,15798 & 
& , 15797,16605, 16606,16607, 16608,15916, 15915], & 
& edgecnc=[5323,5729,5730,5382,5327,5731,5732,5386], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(967),elname="xbrick",eltype="xbrick",typekey=967) 

        call prepare(lib_xbrick(968),key=968, & 
& nodecnc=[2455,2420,2365,808,5027,4992,4937,3380,16609, 16610,16611, 16612,16302, 16301,16613, 16614 & 
& ,16615, 16616,16617, 16618,16310, 16309,16619, 16620], & 
& edgecnc=[5733,5734,5579,5735,5736,5737,5583,5738], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(968),elname="xbrick",eltype="xbrick",typekey=968) 

        call prepare(lib_xbrick(969),key=969, & 
& nodecnc=[2366,730,723,764,4938,3302,3295,3336,16621, 16622,15838, 15837,15828, 15827,16623, 16624,16625 & 
& , 16626,15842, 15841,15836, 15835,16627, 16628], & 
& edgecnc=[5739,5347,5342,5740,5741,5349,5346,5742], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(969),elname="xbrick",eltype="xbrick",typekey=969) 

        call prepare(lib_xbrick(970),key=970, & 
& nodecnc=[2338,747,2375,804,4910,3319,4947,3376,16232, 16231,16629, 16630,16631, 16632,16633, 16634,16236 & 
& , 16235,16635, 16636,16637, 16638,16639, 16640], & 
& edgecnc=[5544,5743,5744,5745,5546,5746,5747,5748], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(970),elname="xbrick",eltype="xbrick",typekey=970) 

        call prepare(lib_xbrick(971),key=971, & 
& nodecnc=[2402,818,2375,2341,4974,3390,4947,4913,16641, 16642,16643, 16644,16645, 16646,16647, 16648 & 
& ,16649, 16650,16651, 16652,16653, 16654,16655, 16656], & 
& edgecnc=[5749,5750,5751,5752,5753,5754,5755,5756], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(971),elname="xbrick",eltype="xbrick",typekey=971) 

        call prepare(lib_xbrick(972),key=972, & 
& nodecnc=[817,766,732,784,3389,3338,3304,3356,16657, 16658,8852, 8851,15806, 15805,16659, 16660,16661 & 
& , 16662,8860, 8859,15810, 15809,16663, 16664], & 
& edgecnc=[5757,1854,5331,5758,5759,1858,5333,5760], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(972),elname="xbrick",eltype="xbrick",typekey=972) 

        call prepare(lib_xbrick(973),key=973, & 
& nodecnc=[2318,2404,835,807,4890,4976,3407,3379,16665, 16666,16667, 16668,16669, 16670,16671, 16672,16673 & 
& , 16674,16675, 16676,16677, 16678,16679, 16680], & 
& edgecnc=[5761,5762,5763,5764,5765,5766,5767,5768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(973),elname="xbrick",eltype="xbrick",typekey=973) 

        call prepare(lib_xbrick(974),key=974, & 
& nodecnc=[851,2510,783,772,3423,5082,3355,3344,16681, 16682,16683, 16684,16256, 16255,8776, 8775,16685 & 
& , 16686,16687, 16688,16264, 16263,8782, 8781], & 
& edgecnc=[5769,5770,5556,1816,5771,5772,5560,1819], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(974),elname="xbrick",eltype="xbrick",typekey=974) 

        call prepare(lib_xbrick(975),key=975, & 
& nodecnc=[821,2319,791,762,3393,4891,3363,3334,16689, 16690,16691, 16692,16562, 16561,16693, 16694,16695 & 
& , 16696,16697, 16698,16564, 16563,16699, 16700], & 
& edgecnc=[5773,5774,5709,5775,5776,5777,5710,5778], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(975),elname="xbrick",eltype="xbrick",typekey=975) 

        call prepare(lib_xbrick(976),key=976, & 
& nodecnc=[713,2346,781,2398,3285,4918,3353,4970,15138, 15137,16326, 16325,16701, 16702,16703, 16704,15146 & 
& , 15145,16332, 16331,16705, 16706,16707, 16708], & 
& edgecnc=[4997,5591,5779,5780,5001,5594,5781,5782], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(976),elname="xbrick",eltype="xbrick",typekey=976) 

        call prepare(lib_xbrick(977),key=977, & 
& nodecnc=[853,808,912,2473,3425,3380,3484,5045,16709, 16710,16711, 16712,16713, 16714,16715, 16716,16717 & 
& , 16718,16719, 16720,16721, 16722,16723, 16724], & 
& edgecnc=[5783,5784,5785,5786,5787,5788,5789,5790], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(977),elname="xbrick",eltype="xbrick",typekey=977) 

        call prepare(lib_xbrick(978),key=978, & 
& nodecnc=[828,782,748,802,3400,3354,3320,3374,16725, 16726,11208, 11207,16182, 16181,16727, 16728,16729 & 
& , 16730,11216, 11215,16186, 16185,16731, 16732], & 
& edgecnc=[5791,3032,5519,5792,5793,3036,5521,5794], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(978),elname="xbrick",eltype="xbrick",typekey=978) 

        call prepare(lib_xbrick(979),key=979, & 
& nodecnc=[859,824,2306,2307,3431,3396,4878,4879,16733, 16734,16172, 16171,16735, 16736,12736, 12735,16737 & 
& , 16738,16180, 16179,16739, 16740,12744, 12743], & 
& edgecnc=[5795,5514,5796,3796,5797,5518,5798,3800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(979),elname="xbrick",eltype="xbrick",typekey=979) 

        call prepare(lib_xbrick(980),key=980, & 
& nodecnc=[855,801,766,817,3427,3373,3338,3389,16741, 16742,16390, 16389,16658, 16657,16743, 16744,16745 & 
& , 16746,16392, 16391,16662, 16661,16747, 16748], & 
& edgecnc=[5799,5623,5757,5800,5801,5624,5759,5802], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(980),elname="xbrick",eltype="xbrick",typekey=980) 

        call prepare(lib_xbrick(981),key=981, & 
& nodecnc=[811,728,2367,2431,3383,3300,4939,5003,16344, 16343,16354, 16353,16476, 16475,16749, 16750,16352 & 
& , 16351,16358, 16357,16480, 16479,16751, 16752], & 
& edgecnc=[5600,5605,5666,5803,5604,5607,5668,5804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(981),elname="xbrick",eltype="xbrick",typekey=981) 

        call prepare(lib_xbrick(982),key=982, & 
& nodecnc=[838,826,867,877,3410,3398,3439,3449,16753, 16754,16755, 16756,16757, 16758,16759, 16760,16761 & 
& , 16762,16763, 16764,16765, 16766,16767, 16768], & 
& edgecnc=[5805,5806,5807,5808,5809,5810,5811,5812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(982),elname="xbrick",eltype="xbrick",typekey=982) 

        call prepare(lib_xbrick(983),key=983, & 
& nodecnc=[819,138,769,809,3391,2710,3341,3381,16769, 16770,16771, 16772,16274, 16273,16508, 16507,16773 & 
& , 16774,16775, 16776,16282, 16281,16514, 16513], & 
& edgecnc=[5813,5814,5565,5682,5815,5816,5569,5685], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(983),elname="xbrick",eltype="xbrick",typekey=983) 

        call prepare(lib_xbrick(984),key=984, & 
& nodecnc=[820,851,735,770,3392,3423,3307,3342,16777, 16778,8774, 8773,15984, 15983,16779, 16780,16781 & 
& , 16782,8780, 8779,15990, 15989,16783, 16784], & 
& edgecnc=[5817,1815,5420,5818,5819,1818,5423,5820], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(984),elname="xbrick",eltype="xbrick",typekey=984) 

        call prepare(lib_xbrick(985),key=985, & 
& nodecnc=[798,770,737,140,3370,3342,3309,2712,16785, 16786,15982, 15981,16034, 16033,16787, 16788,16789 & 
& , 16790,15988, 15987,16036, 16035,16791, 16792], & 
& edgecnc=[5821,5419,5445,5822,5823,5422,5446,5824], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(985),elname="xbrick",eltype="xbrick",typekey=985) 

        call prepare(lib_xbrick(986),key=986, & 
& nodecnc=[2433,2515,2475,734,5005,5087,5047,3306,16252, 16251,16793, 16794,16366, 16365,16414, 16413 & 
& ,16260, 16259,16795, 16796,16372, 16371,16416, 16415], & 
& edgecnc=[5554,5825,5611,5635,5558,5826,5614,5636], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(986),elname="xbrick",eltype="xbrick",typekey=986) 

        call prepare(lib_xbrick(987),key=987, & 
& nodecnc=[812,773,2350,2351,3384,3345,4922,4923,16797, 16798,16482, 16481,16799, 16800,16801, 16802,16803 & 
& , 16804,16486, 16485,16805, 16806,16807, 16808], & 
& edgecnc=[5827,5669,5828,5829,5830,5671,5831,5832], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(987),elname="xbrick",eltype="xbrick",typekey=987) 

        call prepare(lib_xbrick(988),key=988, & 
& nodecnc=[2415,792,2441,774,4987,3364,5013,3346,16809, 16810,16811, 16812,16813, 16814,16815, 16816,16817 & 
& , 16818,16819, 16820,16821, 16822,16823, 16824], & 
& edgecnc=[5833,5834,5835,5836,5837,5838,5839,5840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(988),elname="xbrick",eltype="xbrick",typekey=988) 

        call prepare(lib_xbrick(989),key=989, & 
& nodecnc=[792,812,2351,2441,3364,3384,4923,5013,16825, 16826,16802, 16801,16827, 16828,16812, 16811,16829 & 
& , 16830,16808, 16807,16831, 16832,16820, 16819], & 
& edgecnc=[5841,5829,5842,5834,5843,5832,5844,5838], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(989),elname="xbrick",eltype="xbrick",typekey=989) 

        call prepare(lib_xbrick(990),key=990, & 
& nodecnc=[822,207,2430,793,3394,2779,5002,3365,16833, 16834,16432, 16431,16835, 16836,16837, 16838,16839 & 
& , 16840,16436, 16435,16841, 16842,16843, 16844], & 
& edgecnc=[5845,5644,5846,5847,5848,5646,5849,5850], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(990),elname="xbrick",eltype="xbrick",typekey=990) 

        call prepare(lib_xbrick(991),key=991, & 
& nodecnc=[813,833,793,2405,3385,3405,3365,4977,16845, 16846,12510, 12509,16847, 16848,16849, 16850,16851 & 
& , 16852,12516, 12515,16853, 16854,16855, 16856], & 
& edgecnc=[5851,3683,5852,5853,5854,3686,5855,5856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(991),elname="xbrick",eltype="xbrick",typekey=991) 

        call prepare(lib_xbrick(992),key=992, & 
& nodecnc=[806,813,2405,776,3378,3385,4977,3348,16857, 16858,16850, 16849,16094, 16093,16859, 16860,16861 & 
& , 16862,16856, 16855,16100, 16099,16863, 16864], & 
& edgecnc=[5857,5853,5475,5858,5859,5856,5478,5860], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(992),elname="xbrick",eltype="xbrick",typekey=992) 

        call prepare(lib_xbrick(993),key=993, & 
& nodecnc=[768,806,776,745,3340,3378,3348,3317,16490, 16489,16860, 16859,6640, 6639,16502, 16501,16496 & 
& , 16495,16864, 16863,6646, 6645,16504, 16503], & 
& edgecnc=[5673,5858,748,5679,5676,5860,751,5680], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(993),elname="xbrick",eltype="xbrick",typekey=993) 

        call prepare(lib_xbrick(994),key=994, & 
& nodecnc=[777,796,2432,729,3349,3368,5004,3301,16865, 16866,16867, 16868,16122, 16121,16364, 16363,16869 & 
& , 16870,16871, 16872,16126, 16125,16370, 16369], & 
& edgecnc=[5861,5862,5489,5610,5863,5864,5491,5613], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(994),elname="xbrick",eltype="xbrick",typekey=994) 

        call prepare(lib_xbrick(995),key=995, & 
& nodecnc=[2455,808,853,825,5027,3380,3425,3397,16614, 16613,16710, 16709,16873, 16874,16875, 16876,16620 & 
& , 16619,16718, 16717,16877, 16878,16879, 16880], & 
& edgecnc=[5735,5783,5865,5866,5738,5787,5867,5868], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(995),elname="xbrick",eltype="xbrick",typekey=995) 

        call prepare(lib_xbrick(996),key=996, & 
& nodecnc=[2309,724,755,2364,4881,3296,3327,4936,6570, 6569,15542, 15541,16881, 16882,15796, 15795,6578 & 
& , 6577,15548, 15547,16883, 16884,15804, 15803], & 
& edgecnc=[713,5199,5869,5326,717,5202,5870,5330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(996),elname="xbrick",eltype="xbrick",typekey=996) 

        call prepare(lib_xbrick(997),key=997, & 
& nodecnc=[837,836,794,779,3409,3408,3366,3351,9714, 9713,16885, 16886,16134, 16133,6426, 6425,9722, 9721 & 
& ,16887, 16888,16142, 16141,6434, 6433], & 
& edgecnc=[2285,5871,5495,641,2289,5872,5499,645], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(997),elname="xbrick",eltype="xbrick",typekey=997) 

        call prepare(lib_xbrick(998),key=998, & 
& nodecnc=[780,814,789,749,3352,3386,3361,3321,16889, 16890,16891, 16892,16398, 16397,16216, 16215,16893 & 
& , 16894,16895, 16896,16404, 16403,16220, 16219], & 
& edgecnc=[5873,5874,5627,5536,5875,5876,5630,5538], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(998),elname="xbrick",eltype="xbrick",typekey=998) 

        call prepare(lib_xbrick(999),key=999, & 
& nodecnc=[786,2363,874,2400,3358,4935,3446,4972,16897, 16898,16899, 16900,16901, 16902,16903, 16904,16905 & 
& , 16906,16907, 16908,16909, 16910,16911, 16912], & 
& edgecnc=[5877,5878,5879,5880,5881,5882,5883,5884], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(999),elname="xbrick",eltype="xbrick",typekey=999) 

        call prepare(lib_xbrick(1000),key=1000, & 
& nodecnc=[852,820,770,798,3424,3392,3342,3370,16913, 16914,16780, 16779,16786, 16785,16915, 16916,16917 & 
& , 16918,16784, 16783,16790, 16789,16919, 16920], & 
& edgecnc=[5885,5818,5821,5886,5887,5820,5823,5888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1000),elname="xbrick",eltype="xbrick",typekey=1000) 

        call prepare(lib_xbrick(1001),key=1001, & 
& nodecnc=[910,869,831,872,3482,3441,3403,3444,16921, 16922,16923, 16924,16925, 16926,16927, 16928,16929 & 
& , 16930,16931, 16932,16933, 16934,16935, 16936], & 
& edgecnc=[5889,5890,5891,5892,5893,5894,5895,5896], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1001),elname="xbrick",eltype="xbrick",typekey=1001) 

        call prepare(lib_xbrick(1002),key=1002, & 
& nodecnc=[846,823,2338,804,3418,3395,4910,3376,16937, 16938,16939, 16940,16634, 16633,16941, 16942,16943 & 
& , 16944,16945, 16946,16640, 16639,16947, 16948], & 
& edgecnc=[5897,5898,5745,5899,5900,5901,5748,5902], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1002),elname="xbrick",eltype="xbrick",typekey=1002) 

        call prepare(lib_xbrick(1003),key=1003, & 
& nodecnc=[2363,786,718,754,4935,3358,3290,3326,16898, 16897,16584, 16583,8330, 8329,16949, 16950,16906 & 
& , 16905,16588, 16587,8338, 8337,16951, 16952], & 
& edgecnc=[5877,5720,1593,5903,5881,5722,1597,5904], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1003),elname="xbrick",eltype="xbrick",typekey=1003) 

        call prepare(lib_xbrick(1004),key=1004, & 
& nodecnc=[850,787,789,816,3422,3359,3361,3388,16953, 16954,16400, 16399,16955, 16956,16957, 16958,16959 & 
& , 16960,16406, 16405,16961, 16962,16963, 16964], & 
& edgecnc=[5905,5628,5906,5907,5908,5631,5909,5910], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1004),elname="xbrick",eltype="xbrick",typekey=1004) 

        call prepare(lib_xbrick(1005),key=1005, & 
& nodecnc=[2402,787,850,818,4974,3359,3422,3390,16965, 16966,16954, 16953,16967, 16968,16642, 16641,16969 & 
& , 16970,16960, 16959,16971, 16972,16650, 16649], & 
& edgecnc=[5911,5905,5912,5749,5913,5908,5914,5753], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1005),elname="xbrick",eltype="xbrick",typekey=1005) 

        call prepare(lib_xbrick(1006),key=1006, & 
& nodecnc=[138,2307,788,769,2710,4879,3360,3341,12730, 12729,16973, 16974,16418, 16417,16772, 16771,12738 & 
& , 12737,16975, 16976,16420, 16419,16776, 16775], & 
& edgecnc=[3793,5915,5637,5814,3797,5916,5638,5816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1006),elname="xbrick",eltype="xbrick",typekey=1006) 

        call prepare(lib_xbrick(1007),key=1007, & 
& nodecnc=[2306,799,765,760,4878,3371,3337,3332,16170, 16169,16544, 16543,15886, 15885,16977, 16978,16178 & 
& , 16177,16550, 16549,15892, 15891,16979, 16980], & 
& edgecnc=[5513,5700,5371,5917,5517,5703,5374,5918], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1007),elname="xbrick",eltype="xbrick",typekey=1007) 

        call prepare(lib_xbrick(1008),key=1008, & 
& nodecnc=[814,849,816,789,3386,3421,3388,3361,16981, 16982,8318, 8317,16956, 16955,16892, 16891,16983 & 
& , 16984,8326, 8325,16962, 16961,16896, 16895], & 
& edgecnc=[5919,1587,5906,5874,5920,1591,5909,5876], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1008),elname="xbrick",eltype="xbrick",typekey=1008) 

        call prepare(lib_xbrick(1009),key=1009, & 
& nodecnc=[2366,764,832,790,4938,3336,3404,3362,16624, 16623,16374, 16373,16985, 16986,16298, 16297,16628 & 
& , 16627,16380, 16379,16987, 16988,16306, 16305], & 
& edgecnc=[5740,5615,5921,5577,5742,5618,5922,5581], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1009),elname="xbrick",eltype="xbrick",typekey=1009) 

        call prepare(lib_xbrick(1010),key=1010, & 
& nodecnc=[2318,807,715,750,4890,3379,3287,3322,16672, 16671,16394, 16393,15920, 15919,16989, 16990,16680 & 
& , 16679,16396, 16395,15924, 15923,16991, 16992], & 
& edgecnc=[5764,5625,5388,5923,5768,5626,5390,5924], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1010),elname="xbrick",eltype="xbrick",typekey=1010) 

        call prepare(lib_xbrick(1011),key=1011, & 
& nodecnc=[95,821,762,794,2667,3393,3334,3366,6456, 6455,16694, 16693,16136, 16135,16993, 16994,6464, 6463 & 
& ,16700, 16699,16144, 16143,16995, 16996], & 
& edgecnc=[656,5775,5496,5925,660,5778,5500,5926], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1011),elname="xbrick",eltype="xbrick",typekey=1011) 

        call prepare(lib_xbrick(1012),key=1012, & 
& nodecnc=[2507,796,777,857,5079,3368,3349,3429,16524, 16523,16866, 16865,16997, 16998,16999, 17000,16532 & 
& , 16531,16870, 16869,17001, 17002,17003, 17004], & 
& edgecnc=[5690,5861,5927,5928,5694,5863,5929,5930], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1012),elname="xbrick",eltype="xbrick",typekey=1012) 

        call prepare(lib_xbrick(1013),key=1013, & 
& nodecnc=[857,777,2475,2515,3429,3349,5047,5087,16998, 16997,16362, 16361,16794, 16793,17005, 17006,17002 & 
& , 17001,16368, 16367,16796, 16795,17007, 17008], & 
& edgecnc=[5927,5609,5825,5931,5929,5612,5826,5932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1013),elname="xbrick",eltype="xbrick",typekey=1013) 

        call prepare(lib_xbrick(1014),key=1014, & 
& nodecnc=[274,894,865,2378,2846,3466,3437,4950,17009, 17010,17011, 17012,17013, 17014,17015, 17016,17017 & 
& , 17018,17019, 17020,17021, 17022,17023, 17024], & 
& edgecnc=[5933,5934,5935,5936,5937,5938,5939,5940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1014),elname="xbrick",eltype="xbrick",typekey=1014) 

        call prepare(lib_xbrick(1015),key=1015, & 
& nodecnc=[756,207,822,803,3328,2779,3394,3375,16286, 16285,16834, 16833,17025, 17026,16376, 16375,16292 & 
& , 16291,16840, 16839,17027, 17028,16382, 16381], & 
& edgecnc=[5571,5845,5941,5616,5574,5848,5942,5619], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1015),elname="xbrick",eltype="xbrick",typekey=1015) 

        call prepare(lib_xbrick(1016),key=1016, & 
& nodecnc=[880,95,794,836,3452,2667,3366,3408,17029, 17030,16994, 16993,16886, 16885,17031, 17032,17033 & 
& , 17034,16996, 16995,16888, 16887,17035, 17036], & 
& edgecnc=[5943,5925,5871,5944,5945,5926,5872,5946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1016),elname="xbrick",eltype="xbrick",typekey=1016) 

        call prepare(lib_xbrick(1017),key=1017, & 
& nodecnc=[2456,854,2406,795,5028,3426,4978,3367,17037, 17038,16466, 16465,16222, 16221,17039, 17040,17041 & 
& , 17042,16472, 16471,16226, 16225,17043, 17044], & 
& edgecnc=[5947,5661,5539,5948,5949,5664,5541,5950], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1017),elname="xbrick",eltype="xbrick",typekey=1017) 

        call prepare(lib_xbrick(1018),key=1018, & 
& nodecnc=[2432,796,844,771,5004,3368,3416,3343,16868, 16867,16522, 16521,12752, 12751,16110, 16109,16872 & 
& , 16871,16530, 16529,12760, 12759,16116, 16115], & 
& edgecnc=[5862,5689,3804,5483,5864,5693,3808,5486], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1018),elname="xbrick",eltype="xbrick",typekey=1018) 

        call prepare(lib_xbrick(1019),key=1019, & 
& nodecnc=[886,122,797,854,3458,2694,3369,3426,17045, 17046,17047, 17048,16462, 16461,17049, 17050,17051 & 
& , 17052,17053, 17054,16468, 16467,17055, 17056], & 
& edgecnc=[5951,5952,5659,5953,5954,5955,5662,5956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1019),elname="xbrick",eltype="xbrick",typekey=1019) 

        call prepare(lib_xbrick(1020),key=1020, & 
& nodecnc=[2334,924,936,900,4906,3496,3508,3472,17057, 17058,17059, 17060,17061, 17062,17063, 17064,17065 & 
& , 17066,17067, 17068,17069, 17070,17071, 17072], & 
& edgecnc=[5957,5958,5959,5960,5961,5962,5963,5964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1020),elname="xbrick",eltype="xbrick",typekey=1020) 

        call prepare(lib_xbrick(1021),key=1021, & 
& nodecnc=[883,839,799,862,3455,3411,3371,3434,17073, 17074,16546, 16545,16168, 16167,17075, 17076,17077 & 
& , 17078,16552, 16551,16176, 16175,17079, 17080], & 
& edgecnc=[5965,5701,5512,5966,5967,5704,5516,5968], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1021),elname="xbrick",eltype="xbrick",typekey=1021) 

        call prepare(lib_xbrick(1022),key=1022, & 
& nodecnc=[2415,774,2424,2454,4987,3346,4996,5026,16816, 16815,16438, 16437,8352, 8351,17081, 17082,16824 & 
& , 16823,16444, 16443,8360, 8359,17083, 17084], & 
& edgecnc=[5836,5647,1604,5969,5840,5650,1608,5970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1022),elname="xbrick",eltype="xbrick",typekey=1022) 

        call prepare(lib_xbrick(1023),key=1023, & 
& nodecnc=[864,798,140,830,3436,3370,2712,3402,17085, 17086,16788, 16787,11222, 11221,17087, 17088,17089 & 
& , 17090,16792, 16791,11230, 11229,17091, 17092], & 
& edgecnc=[5971,5822,3039,5972,5973,5824,3043,5974], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1023),elname="xbrick",eltype="xbrick",typekey=1023) 

        call prepare(lib_xbrick(1024),key=1024, & 
& nodecnc=[2334,830,801,855,4906,3402,3373,3427,17093, 17094,11220, 11219,16742, 16741,17095, 17096,17097 & 
& , 17098,11228, 11227,16746, 16745,17099, 17100], & 
& edgecnc=[5975,3038,5799,5976,5977,3042,5801,5978], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1024),elname="xbrick",eltype="xbrick",typekey=1024) 

        call prepare(lib_xbrick(1025),key=1025, & 
& nodecnc=[711,782,907,829,3283,3354,3479,3401,11202, 11201,17101, 17102,17103, 17104,16534, 16533,11210 & 
& , 11209,17105, 17106,17107, 17108,16538, 16537], & 
& edgecnc=[3029,5979,5980,5695,3033,5981,5982,5697], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1025),elname="xbrick",eltype="xbrick",typekey=1025) 

        call prepare(lib_xbrick(1026),key=1026, & 
& nodecnc=[875,828,802,839,3447,3400,3374,3411,17109, 17110,16728, 16727,16542, 16541,17111, 17112,17113 & 
& , 17114,16732, 16731,16548, 16547,17115, 17116], & 
& edgecnc=[5983,5792,5699,5984,5985,5794,5702,5986], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1026),elname="xbrick",eltype="xbrick",typekey=1026) 

        call prepare(lib_xbrick(1027),key=1027, & 
& nodecnc=[822,866,847,803,3394,3438,3419,3375,17117, 17118,17119, 17120,17121, 17122,17026, 17025,17123 & 
& , 17124,17125, 17126,17127, 17128,17028, 17027], & 
& edgecnc=[5987,5988,5989,5941,5990,5991,5992,5942], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1027),elname="xbrick",eltype="xbrick",typekey=1027) 

        call prepare(lib_xbrick(1028),key=1028, & 
& nodecnc=[805,811,2431,773,3377,3383,5003,3345,17129, 17130,16750, 16749,16484, 16483,17131, 17132,17133 & 
& , 17134,16752, 16751,16488, 16487,17135, 17136], & 
& edgecnc=[5993,5803,5670,5994,5995,5804,5672,5996], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1028),elname="xbrick",eltype="xbrick",typekey=1028) 

        call prepare(lib_xbrick(1029),key=1029, & 
& nodecnc=[840,841,813,806,3412,3413,3385,3378,17137, 17138,17139, 17140,16858, 16857,16494, 16493,17141 & 
& , 17142,17143, 17144,16862, 16861,16500, 16499], & 
& edgecnc=[5997,5998,5857,5675,5999,6000,5859,5678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1029),elname="xbrick",eltype="xbrick",typekey=1029) 

        call prepare(lib_xbrick(1030),key=1030, & 
& nodecnc=[869,817,784,831,3441,3389,3356,3403,17145, 17146,16660, 16659,15114, 15113,16924, 16923,17147 & 
& , 17148,16664, 16663,15122, 15121,16932, 16931], & 
& edgecnc=[6001,5758,4985,5890,6002,5760,4989,5894], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1030),elname="xbrick",eltype="xbrick",typekey=1030) 

        call prepare(lib_xbrick(1031),key=1031, & 
& nodecnc=[807,835,872,831,3379,3407,3444,3403,16670, 16669,17149, 17150,16926, 16925,15120, 15119,16678 & 
& , 16677,17151, 17152,16934, 16933,15128, 15127], & 
& edgecnc=[5763,6003,5891,4988,5767,6004,5895,4992], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1031),elname="xbrick",eltype="xbrick",typekey=1031) 

        call prepare(lib_xbrick(1032),key=1032, & 
& nodecnc=[825,2456,795,778,3397,5028,3367,3350,17153, 17154,17040, 17039,15902, 15901,17155, 17156,17157 & 
& , 17158,17044, 17043,15910, 15909,17159, 17160], & 
& edgecnc=[6005,5948,5379,6006,6007,5950,5383,6008], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1032),elname="xbrick",eltype="xbrick",typekey=1032) 

        call prepare(lib_xbrick(1033),key=1033, & 
& nodecnc=[843,845,810,811,3415,3417,3382,3383,17161, 17162,17163, 17164,16338, 16337,17165, 17166,17167 & 
& , 17168,17169, 17170,16346, 16345,17171, 17172], & 
& edgecnc=[6009,6010,5597,6011,6012,6013,5601,6014], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1033),elname="xbrick",eltype="xbrick",typekey=1033) 

        call prepare(lib_xbrick(1034),key=1034, & 
& nodecnc=[811,805,2353,843,3383,3377,4925,3415,17130, 17129,12492, 12491,17173, 17174,17166, 17165,17134 & 
& , 17133,12500, 12499,17175, 17176,17172, 17171], & 
& edgecnc=[5993,3674,6015,6011,5995,3678,6016,6014], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1034),elname="xbrick",eltype="xbrick",typekey=1034) 

        call prepare(lib_xbrick(1035),key=1035, & 
& nodecnc=[792,838,2460,812,3364,3410,5032,3384,17177, 17178,12496, 12495,17179, 17180,16826, 16825,17181 & 
& , 17182,12504, 12503,17183, 17184,16830, 16829], & 
& edgecnc=[6017,3676,6018,5841,6019,3680,6020,5843], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1035),elname="xbrick",eltype="xbrick",typekey=1035) 

        call prepare(lib_xbrick(1036),key=1036, & 
& nodecnc=[841,873,833,813,3413,3445,3405,3385,17185, 17186,6554, 6553,16846, 16845,17140, 17139,17187 & 
& , 17188,6562, 6561,16852, 16851,17144, 17143], & 
& edgecnc=[6021,705,5851,5998,6022,709,5854,6000], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1036),elname="xbrick",eltype="xbrick",typekey=1036) 

        call prepare(lib_xbrick(1037),key=1037, & 
& nodecnc=[978,868,829,907,3550,3440,3401,3479,17189, 17190,16556, 16555,17104, 17103,17191, 17192,17193 & 
& , 17194,16560, 16559,17108, 17107,17195, 17196], & 
& edgecnc=[6023,5706,5980,6024,6025,5708,5982,6026], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1037),elname="xbrick",eltype="xbrick",typekey=1037) 

        call prepare(lib_xbrick(1038),key=1038, & 
& nodecnc=[2409,874,2363,823,4981,3446,4935,3395,17197, 17198,16900, 16899,17199, 17200,17201, 17202,17203 & 
& , 17204,16908, 16907,17205, 17206,17207, 17208], & 
& edgecnc=[6027,5878,6028,6029,6030,5882,6031,6032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1038),elname="xbrick",eltype="xbrick",typekey=1038) 

        call prepare(lib_xbrick(1039),key=1039, & 
& nodecnc=[2398,781,2416,763,4970,3353,4988,3335,16702, 16701,16568, 16567,17209, 17210,17211, 17212,16706 & 
& , 16705,16576, 16575,17213, 17214,17215, 17216], & 
& edgecnc=[5779,5712,6033,6034,5781,5716,6035,6036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1039),elname="xbrick",eltype="xbrick",typekey=1039) 

        call prepare(lib_xbrick(1040),key=1040, & 
& nodecnc=[2538,918,850,816,5110,3490,3422,3388,17217, 17218,17219, 17220,16958, 16957,8316, 8315,17221 & 
& , 17222,17223, 17224,16964, 16963,8324, 8323], & 
& edgecnc=[6037,6038,5907,1586,6039,6040,5910,1590], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1040),elname="xbrick",eltype="xbrick",typekey=1040) 

        call prepare(lib_xbrick(1041),key=1041, & 
& nodecnc=[849,919,884,882,3421,3491,3456,3454,17225, 17226,17227, 17228,17229, 17230,8320, 8319,17231 & 
& , 17232,17233, 17234,17235, 17236,8328, 8327], & 
& edgecnc=[6041,6042,6043,1588,6044,6045,6046,1592], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1041),elname="xbrick",eltype="xbrick",typekey=1041) 

        call prepare(lib_xbrick(1042),key=1042, & 
& nodecnc=[911,855,817,869,3483,3427,3389,3441,17237, 17238,16744, 16743,17146, 17145,17239, 17240,17241 & 
& , 17242,16748, 16747,17148, 17147,17243, 17244], & 
& edgecnc=[6047,5800,6001,6048,6049,5802,6002,6050], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1042),elname="xbrick",eltype="xbrick",typekey=1042) 

        call prepare(lib_xbrick(1043),key=1043, & 
& nodecnc=[850,937,2410,818,3422,3509,4982,3390,17245, 17246,17247, 17248,17249, 17250,16968, 16967,17251 & 
& , 17252,17253, 17254,17255, 17256,16972, 16971], & 
& edgecnc=[6051,6052,6053,5912,6054,6055,6056,5914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1043),elname="xbrick",eltype="xbrick",typekey=1043) 

        call prepare(lib_xbrick(1044),key=1044, & 
& nodecnc=[808,790,832,912,3380,3362,3404,3484,16300, 16299,16986, 16985,17257, 17258,16712, 16711,16308 & 
& , 16307,16988, 16987,17259, 17260,16720, 16719], & 
& edgecnc=[5578,5921,6057,5784,5582,5922,6058,5788], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1044),elname="xbrick",eltype="xbrick",typekey=1044) 

        call prepare(lib_xbrick(1045),key=1045, & 
& nodecnc=[848,138,819,890,3420,2710,3391,3462,12732, 12731,16770, 16769,17261, 17262,17263, 17264,12740 & 
& , 12739,16774, 16773,17265, 17266,17267, 17268], & 
& edgecnc=[3794,5813,6059,6060,3798,5815,6061,6062], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1045),elname="xbrick",eltype="xbrick",typekey=1045) 

        call prepare(lib_xbrick(1046),key=1046, & 
& nodecnc=[2510,858,842,783,5082,3430,3414,3355,17269, 17270,17271, 17272,17273, 17274,16684, 16683,17275 & 
& , 17276,17277, 17278,17279, 17280,16688, 16687], & 
& edgecnc=[6063,6064,6065,5770,6066,6067,6068,5772], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1046),elname="xbrick",eltype="xbrick",typekey=1046) 

        call prepare(lib_xbrick(1047),key=1047, & 
& nodecnc=[899,858,820,852,3471,3430,3392,3424,17281, 17282,17283, 17284,16914, 16913,17285, 17286,17287 & 
& , 17288,17289, 17290,16918, 16917,17291, 17292], & 
& edgecnc=[6069,6070,5885,6071,6072,6073,5887,6074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1047),elname="xbrick",eltype="xbrick",typekey=1047) 

        call prepare(lib_xbrick(1048),key=1048, & 
& nodecnc=[783,842,857,2515,3355,3414,3429,5087,17274, 17273,17293, 17294,17006, 17005,16250, 16249,17280 & 
& , 17279,17295, 17296,17008, 17007,16258, 16257], & 
& edgecnc=[6065,6075,5931,5553,6068,6076,5932,5557], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1048),elname="xbrick",eltype="xbrick",typekey=1048) 

        call prepare(lib_xbrick(1049),key=1049, & 
& nodecnc=[857,931,876,2507,3429,3503,3448,5079,17297, 17298,17299, 17300,16518, 16517,17000, 16999,17301 & 
& , 17302,17303, 17304,16526, 16525,17004, 17003], & 
& edgecnc=[6077,6078,5687,5928,6079,6080,5691,5930], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1049),elname="xbrick",eltype="xbrick",typekey=1049) 

        call prepare(lib_xbrick(1050),key=1050, & 
& nodecnc=[898,860,904,926,3470,3432,3476,3498,17305, 17306,12506, 12505,17307, 17308,17309, 17310,17311 & 
& , 17312,12512, 12511,17313, 17314,17315, 17316], & 
& edgecnc=[6081,3681,6082,6083,6084,3684,6085,6086], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1050),elname="xbrick",eltype="xbrick",typekey=1050) 

        call prepare(lib_xbrick(1051),key=1051, & 
& nodecnc=[752,786,2400,827,3324,3358,4972,3399,16582, 16581,16904, 16903,17317, 17318,15856, 15855,16586 & 
& , 16585,16912, 16911,17319, 17320,15864, 15863], & 
& edgecnc=[5719,5880,6087,5356,5721,5884,6088,5360], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1051),elname="xbrick",eltype="xbrick",typekey=1051) 

        call prepare(lib_xbrick(1052),key=1052, & 
& nodecnc=[890,906,859,848,3462,3478,3431,3420,17321, 17322,17323, 17324,12734, 12733,17264, 17263,17325 & 
& , 17326,17327, 17328,12742, 12741,17268, 17267], & 
& edgecnc=[6089,6090,3795,6060,6091,6092,3799,6062], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1052),elname="xbrick",eltype="xbrick",typekey=1052) 

        call prepare(lib_xbrick(1053),key=1053, & 
& nodecnc=[2454,865,894,2415,5026,3437,3466,4987,17329, 17330,17012, 17011,17331, 17332,17082, 17081,17333 & 
& , 17334,17020, 17019,17335, 17336,17084, 17083], & 
& edgecnc=[6093,5934,6094,5969,6095,5938,6096,5970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1053),elname="xbrick",eltype="xbrick",typekey=1053) 

        call prepare(lib_xbrick(1054),key=1054, & 
& nodecnc=[827,871,2527,2469,3399,3443,5099,5041,17337, 17338,17339, 17340,17341, 17342,17343, 17344,17345 & 
& , 17346,17347, 17348,17349, 17350,17351, 17352], & 
& edgecnc=[6097,6098,6099,6100,6101,6102,6103,6104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1054),elname="xbrick",eltype="xbrick",typekey=1054) 

        call prepare(lib_xbrick(1055),key=1055, & 
& nodecnc=[892,2478,2400,874,3464,5050,4972,3446,17353, 17354,17355, 17356,16902, 16901,17357, 17358,17359 & 
& , 17360,17361, 17362,16910, 16909,17363, 17364], & 
& edgecnc=[6105,6106,5879,6107,6108,6109,5883,6110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1055),elname="xbrick",eltype="xbrick",typekey=1055) 

        call prepare(lib_xbrick(1056),key=1056, & 
& nodecnc=[828,875,921,896,3400,3447,3493,3468,17110, 17109,17365, 17366,11162, 11161,17367, 17368,17114 & 
& , 17113,17369, 17370,11168, 11167,17371, 17372], & 
& edgecnc=[5983,6111,3009,6112,5985,6113,3012,6114], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1056),elname="xbrick",eltype="xbrick",typekey=1056) 

        call prepare(lib_xbrick(1057),key=1057, & 
& nodecnc=[958,814,780,868,3530,3386,3352,3440,17373, 17374,16890, 16889,16554, 16553,17375, 17376,17377 & 
& , 17378,16894, 16893,16558, 16557,17379, 17380], & 
& edgecnc=[6115,5873,5705,6116,6117,5875,5707,6118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1057),elname="xbrick",eltype="xbrick",typekey=1057) 

        call prepare(lib_xbrick(1058),key=1058, & 
& nodecnc=[895,852,798,864,3467,3424,3370,3436,17381, 17382,16916, 16915,17086, 17085,11240, 11239,17383 & 
& , 17384,16920, 16919,17090, 17089,11248, 11247], & 
& edgecnc=[6119,5886,5971,3048,6120,5888,5973,3052], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1058),elname="xbrick",eltype="xbrick",typekey=1058) 

        call prepare(lib_xbrick(1059),key=1059, & 
& nodecnc=[2334,900,864,830,4906,3472,3436,3402,17064, 17063,11234, 11233,17088, 17087,17094, 17093,17072 & 
& , 17071,11242, 11241,17092, 17091,17098, 17097], & 
& edgecnc=[5960,3045,5972,5975,5964,3049,5974,5977], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1059),elname="xbrick",eltype="xbrick",typekey=1059) 

        call prepare(lib_xbrick(1060),key=1060, & 
& nodecnc=[912,832,803,847,3484,3404,3375,3419,17258, 17257,16378, 16377,17122, 17121,17385, 17386,17260 & 
& , 17259,16384, 16383,17128, 17127,17387, 17388], & 
& edgecnc=[6057,5617,5989,6121,6058,5620,5992,6122], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1060),elname="xbrick",eltype="xbrick",typekey=1060) 

        call prepare(lib_xbrick(1061),key=1061, & 
& nodecnc=[866,822,793,860,3438,3394,3365,3432,17118, 17117,16838, 16837,12508, 12507,17389, 17390,17124 & 
& , 17123,16844, 16843,12514, 12513,17391, 17392], & 
& edgecnc=[5987,5847,3682,6123,5990,5850,3685,6124], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1061),elname="xbrick",eltype="xbrick",typekey=1061) 

        call prepare(lib_xbrick(1062),key=1062, & 
& nodecnc=[856,903,2429,834,3428,3475,5001,3406,17393, 17394,17395, 17396,17397, 17398,12064, 12063,17399 & 
& , 17400,17401, 17402,17403, 17404,12072, 12071], & 
& edgecnc=[6125,6126,6127,3460,6128,6129,6130,3464], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1062),elname="xbrick",eltype="xbrick",typekey=1062) 

        call prepare(lib_xbrick(1063),key=1063, & 
& nodecnc=[861,2397,2319,821,3433,4969,4891,3393,6438, 6437,17405, 17406,16690, 16689,6454, 6453,6446 & 
& , 6445,17407, 17408,16696, 16695,6462, 6461], & 
& edgecnc=[647,6131,5773,655,651,6132,5776,659], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1063),elname="xbrick",eltype="xbrick",typekey=1063) 

        call prepare(lib_xbrick(1064),key=1064, & 
& nodecnc=[2318,2319,2397,2404,4890,4891,4969,4976,17409, 17410,17406, 17405,6444, 6443,16666, 16665,17411 & 
& , 17412,17408, 17407,6452, 6451,16674, 16673], & 
& edgecnc=[6133,6131,650,5761,6134,6132,654,5765], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1064),elname="xbrick",eltype="xbrick",typekey=1064) 

        call prepare(lib_xbrick(1065),key=1065, & 
& nodecnc=[908,880,836,870,3480,3452,3408,3442,17413, 17414,17032, 17031,9712, 9711,17415, 17416,17417 & 
& , 17418,17036, 17035,9720, 9719,17419, 17420], & 
& edgecnc=[6135,5944,2284,6136,6137,5946,2288,6138], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1065),elname="xbrick",eltype="xbrick",typekey=1065) 

        call prepare(lib_xbrick(1066),key=1066, & 
& nodecnc=[967,946,122,922,3539,3518,2694,3494,17421, 17422,17423, 17424,17425, 17426,17427, 17428,17429 & 
& , 17430,17431, 17432,17433, 17434,17435, 17436], & 
& edgecnc=[6139,6140,6141,6142,6143,6144,6145,6146], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1066),elname="xbrick",eltype="xbrick",typekey=1066) 

        call prepare(lib_xbrick(1067),key=1067, & 
& nodecnc=[952,870,946,927,3524,3442,3518,3499,17437, 17438,9710, 9709,17439, 17440,17441, 17442,17443 & 
& , 17444,9718, 9717,17445, 17446,17447, 17448], & 
& edgecnc=[6147,2283,6148,6149,6150,2287,6151,6152], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1067),elname="xbrick",eltype="xbrick",typekey=1067) 

        call prepare(lib_xbrick(1068),key=1068, & 
& nodecnc=[946,837,797,122,3518,3409,3369,2694,9716, 9715,6424, 6423,17048, 17047,17424, 17423,9724, 9723 & 
& ,6432, 6431,17054, 17053,17432, 17431], & 
& edgecnc=[2286,640,5952,6140,2290,644,5955,6144], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1068),elname="xbrick",eltype="xbrick",typekey=1068) 

        call prepare(lib_xbrick(1069),key=1069, & 
& nodecnc=[2327,2353,838,877,4899,4925,3410,3449,17449, 17450,12490, 12489,16760, 16759,17451, 17452,17453 & 
& , 17454,12498, 12497,16768, 16767,17455, 17456], & 
& edgecnc=[6153,3673,5808,6154,6155,3677,5812,6156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1069),elname="xbrick",eltype="xbrick",typekey=1069) 

        call prepare(lib_xbrick(1070),key=1070, & 
& nodecnc=[950,875,839,883,3522,3447,3411,3455,17457, 17458,17112, 17111,17074, 17073,17459, 17460,17461 & 
& , 17462,17116, 17115,17078, 17077,17463, 17464], & 
& edgecnc=[6157,5984,5965,6158,6159,5986,5967,6160], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1070),elname="xbrick",eltype="xbrick",typekey=1070) 

        call prepare(lib_xbrick(1071),key=1071, & 
& nodecnc=[879,878,841,840,3451,3450,3413,3412,6606, 6605,17465, 17466,17138, 17137,17467, 17468,6614 & 
& , 6613,17469, 17470,17142, 17141,17471, 17472], & 
& edgecnc=[731,6161,5997,6162,735,6163,5999,6164], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1071),elname="xbrick",eltype="xbrick",typekey=1071) 

        call prepare(lib_xbrick(1072),key=1072, & 
& nodecnc=[810,845,879,840,3382,3417,3451,3412,17164, 17163,17473, 17474,17468, 17467,16492, 16491,17170 & 
& , 17169,17475, 17476,17472, 17471,16498, 16497], & 
& edgecnc=[6010,6165,6162,5674,6013,6166,6164,5677], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1072),elname="xbrick",eltype="xbrick",typekey=1072) 

        call prepare(lib_xbrick(1073),key=1073, & 
& nodecnc=[878,947,873,841,3450,3519,3445,3413,17477, 17478,17479, 17480,17186, 17185,17466, 17465,17481 & 
& , 17482,17483, 17484,17188, 17187,17470, 17469], & 
& edgecnc=[6167,6168,6021,6161,6169,6170,6022,6163], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1073),elname="xbrick",eltype="xbrick",typekey=1073) 

        call prepare(lib_xbrick(1074),key=1074, & 
& nodecnc=[842,858,899,891,3414,3430,3471,3463,17272, 17271,17282, 17281,17485, 17486,17487, 17488,17278 & 
& , 17277,17288, 17287,17489, 17490,17491, 17492], & 
& edgecnc=[6064,6069,6171,6172,6067,6072,6173,6174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1074),elname="xbrick",eltype="xbrick",typekey=1074) 

        call prepare(lib_xbrick(1075),key=1075, & 
& nodecnc=[902,925,863,2434,3474,3497,3435,5006,17493, 17494,17495, 17496,17497, 17498,17499, 17500,17501 & 
& , 17502,17503, 17504,17505, 17506,17507, 17508], & 
& edgecnc=[6175,6176,6177,6178,6179,6180,6181,6182], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1075),elname="xbrick",eltype="xbrick",typekey=1075) 

        call prepare(lib_xbrick(1076),key=1076, & 
& nodecnc=[275,879,845,881,2847,3451,3417,3453,6608, 6607,17474, 17473,17509, 17510,12324, 12323,6616 & 
& , 6615,17476, 17475,17511, 17512,12332, 12331], & 
& edgecnc=[732,6165,6183,3590,736,6166,6184,3594], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1076),elname="xbrick",eltype="xbrick",typekey=1076) 

        call prepare(lib_xbrick(1077),key=1077, & 
& nodecnc=[175,937,850,918,2747,3509,3422,3490,17513, 17514,17246, 17245,17220, 17219,17515, 17516,17517 & 
& , 17518,17252, 17251,17224, 17223,17519, 17520], & 
& edgecnc=[6185,6051,6038,6186,6187,6054,6040,6188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1077),elname="xbrick",eltype="xbrick",typekey=1077) 

        call prepare(lib_xbrick(1078),key=1078, & 
& nodecnc=[937,846,804,2410,3509,3418,3376,4982,17521, 17522,16942, 16941,17523, 17524,17248, 17247,17525 & 
& , 17526,16948, 16947,17527, 17528,17254, 17253], & 
& edgecnc=[6189,5899,6190,6052,6191,5902,6192,6055], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1078),elname="xbrick",eltype="xbrick",typekey=1078) 

        call prepare(lib_xbrick(1079),key=1079, & 
& nodecnc=[898,901,912,847,3470,3473,3484,3419,17529, 17530,17531, 17532,17386, 17385,17533, 17534,17535 & 
& , 17536,17537, 17538,17388, 17387,17539, 17540], & 
& edgecnc=[6193,6194,6121,6195,6196,6197,6122,6198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1079),elname="xbrick",eltype="xbrick",typekey=1079) 

        call prepare(lib_xbrick(1080),key=1080, & 
& nodecnc=[819,2480,863,890,3391,5052,3435,3462,16506, 16505,17541, 17542,17543, 17544,17262, 17261,16512 & 
& , 16511,17545, 17546,17547, 17548,17266, 17265], & 
& edgecnc=[5681,6199,6200,6059,5684,6201,6202,6061], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1080),elname="xbrick",eltype="xbrick",typekey=1080) 

        call prepare(lib_xbrick(1081),key=1081, & 
& nodecnc=[849,814,958,919,3421,3386,3530,3491,16982, 16981,17374, 17373,17549, 17550,17226, 17225,16984 & 
& , 16983,17378, 17377,17551, 17552,17232, 17231], & 
& edgecnc=[5919,6115,6203,6041,5920,6117,6204,6044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1081),elname="xbrick",eltype="xbrick",typekey=1081) 

        call prepare(lib_xbrick(1082),key=1082, & 
& nodecnc=[915,938,918,2538,3487,3510,3490,5110,17553, 17554,17555, 17556,17218, 17217,17557, 17558,17559 & 
& , 17560,17561, 17562,17222, 17221,17563, 17564], & 
& edgecnc=[6205,6206,6037,6207,6208,6209,6039,6210], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1082),elname="xbrick",eltype="xbrick",typekey=1082) 

        call prepare(lib_xbrick(1083),key=1083, & 
& nodecnc=[854,2456,2511,886,3426,5028,5083,3458,17038, 17037,17565, 17566,17567, 17568,17050, 17049,17042 & 
& , 17041,17569, 17570,17571, 17572,17056, 17055], & 
& edgecnc=[5947,6211,6212,5953,5949,6213,6214,5956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1083),elname="xbrick",eltype="xbrick",typekey=1083) 

        call prepare(lib_xbrick(1084),key=1084, & 
& nodecnc=[956,2517,893,2544,3528,5089,3465,5116,17573, 17574,17575, 17576,17577, 17578,17579, 17580,17581 & 
& , 17582,17583, 17584,17585, 17586,17587, 17588], & 
& edgecnc=[6215,6216,6217,6218,6219,6220,6221,6222], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1084),elname="xbrick",eltype="xbrick",typekey=1084) 

        call prepare(lib_xbrick(1085),key=1085, & 
& nodecnc=[893,856,800,815,3465,3428,3372,3387,17589, 17590,12062, 12061,16594, 16593,17591, 17592,17593 & 
& , 17594,12070, 12069,16598, 16597,17595, 17596], & 
& edgecnc=[6223,3459,5725,6224,6225,3463,5727,6226], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1085),elname="xbrick",eltype="xbrick",typekey=1085) 

        call prepare(lib_xbrick(1086),key=1086, & 
& nodecnc=[1050,931,891,920,3622,3503,3463,3492,12968, 12967,17597, 17598,17599, 17600,8748, 8747,12976 & 
& , 12975,17601, 17602,17603, 17604,8756, 8755], & 
& edgecnc=[3912,6227,6228,1802,3916,6229,6230,1806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1086),elname="xbrick",eltype="xbrick",typekey=1086) 

        call prepare(lib_xbrick(1087),key=1087, & 
& nodecnc=[851,820,858,2510,3423,3392,3430,5082,16778, 16777,17284, 17283,17270, 17269,16682, 16681,16782 & 
& , 16781,17290, 17289,17276, 17275,16686, 16685], & 
& edgecnc=[5817,6070,6063,5769,5819,6073,6066,5771], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1087),elname="xbrick",eltype="xbrick",typekey=1087) 

        call prepare(lib_xbrick(1088),key=1088, & 
& nodecnc=[883,862,2435,888,3455,3434,5007,3460,17076, 17075,17605, 17606,17607, 17608,17609, 17610,17080 & 
& , 17079,17611, 17612,17613, 17614,17615, 17616], & 
& edgecnc=[5966,6231,6232,6233,5968,6234,6235,6236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1088),elname="xbrick",eltype="xbrick",typekey=1088) 

        call prepare(lib_xbrick(1089),key=1089, & 
& nodecnc=[847,866,860,898,3419,3438,3432,3470,17120, 17119,17390, 17389,17306, 17305,17534, 17533,17126 & 
& , 17125,17392, 17391,17312, 17311,17540, 17539], & 
& edgecnc=[5988,6123,6081,6195,5991,6124,6084,6198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1089),elname="xbrick",eltype="xbrick",typekey=1089) 

        call prepare(lib_xbrick(1090),key=1090, & 
& nodecnc=[933,2497,959,994,3505,5069,3531,3566,17617, 17618,17619, 17620,17621, 17622,17623, 17624,17625 & 
& , 17626,17627, 17628,17629, 17630,17631, 17632], & 
& edgecnc=[6237,6238,6239,6240,6241,6242,6243,6244], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1090),elname="xbrick",eltype="xbrick",typekey=1090) 

        call prepare(lib_xbrick(1091),key=1091, & 
& nodecnc=[970,902,2434,876,3542,3474,5006,3448,17633, 17634,17500, 17499,17635, 17636,17637, 17638,17639 & 
& , 17640,17508, 17507,17641, 17642,17643, 17644], & 
& edgecnc=[6245,6178,6246,6247,6248,6182,6249,6250], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1091),elname="xbrick",eltype="xbrick",typekey=1091) 

        call prepare(lib_xbrick(1092),key=1092, & 
& nodecnc=[2415,894,867,826,4987,3466,3439,3398,17332, 17331,17645, 17646,16756, 16755,17647, 17648,17336 & 
& , 17335,17649, 17650,16764, 16763,17651, 17652], & 
& edgecnc=[6094,6251,5806,6252,6096,6253,5810,6254], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1092),elname="xbrick",eltype="xbrick",typekey=1092) 

        call prepare(lib_xbrick(1093),key=1093, & 
& nodecnc=[916,951,903,2517,3488,3523,3475,5089,17653, 17654,17655, 17656,17657, 17658,17659, 17660,17661 & 
& , 17662,17663, 17664,17665, 17666,17667, 17668], & 
& edgecnc=[6255,6256,6257,6258,6259,6260,6261,6262], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1093),elname="xbrick",eltype="xbrick",typekey=1093) 

        call prepare(lib_xbrick(1094),key=1094, & 
& nodecnc=[999,867,894,274,3571,3439,3466,2846,17669, 17670,17646, 17645,17010, 17009,8362, 8361,17671 & 
& , 17672,17650, 17649,17018, 17017,8370, 8369], & 
& edgecnc=[6263,6251,5933,1609,6264,6253,5937,1613], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1094),elname="xbrick",eltype="xbrick",typekey=1094) 

        call prepare(lib_xbrick(1095),key=1095, & 
& nodecnc=[870,952,2495,908,3442,3524,5067,3480,17438, 17437,17673, 17674,17675, 17676,17416, 17415,17444 & 
& , 17443,17677, 17678,17679, 17680,17420, 17419], & 
& edgecnc=[6147,6265,6266,6136,6150,6267,6268,6138], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1095),elname="xbrick",eltype="xbrick",typekey=1095) 

        call prepare(lib_xbrick(1096),key=1096, & 
& nodecnc=[874,2409,913,892,3446,4981,3485,3464,17198, 17197,17681, 17682,11850, 11849,17358, 17357,17204 & 
& , 17203,17683, 17684,11858, 11857,17364, 17363], & 
& edgecnc=[6027,6269,3353,6107,6030,6270,3357,6110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1096),elname="xbrick",eltype="xbrick",typekey=1096) 

        call prepare(lib_xbrick(1097),key=1097, & 
& nodecnc=[887,959,909,861,3459,3531,3481,3433,17685, 17686,17687, 17688,6440, 6439,6460, 6459,17689, 17690 & 
& ,17691, 17692,6448, 6447,6468, 6467], & 
& edgecnc=[6271,6272,648,658,6273,6274,652,662], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1097),elname="xbrick",eltype="xbrick",typekey=1097) 

        call prepare(lib_xbrick(1098),key=1098, & 
& nodecnc=[977,276,873,947,3549,2848,3445,3519,17693, 17694,6556, 6555,17480, 17479,12344, 12343,17695 & 
& , 17696,6564, 6563,17484, 17483,12352, 12351], & 
& edgecnc=[6275,706,6168,3600,6276,710,6170,3604], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1098),elname="xbrick",eltype="xbrick",typekey=1098) 

        call prepare(lib_xbrick(1099),key=1099, & 
& nodecnc=[2479,913,2409,885,5051,3485,4981,3457,17697, 17698,17682, 17681,17699, 17700,17701, 17702,17703 & 
& , 17704,17684, 17683,17705, 17706,17707, 17708], & 
& edgecnc=[6277,6269,6278,6279,6280,6270,6281,6282], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1099),elname="xbrick",eltype="xbrick",typekey=1099) 

        call prepare(lib_xbrick(1100),key=1100, & 
& nodecnc=[827,2400,2478,871,3399,4972,5050,3443,17318, 17317,17356, 17355,17709, 17710,17338, 17337,17320 & 
& , 17319,17362, 17361,17711, 17712,17346, 17345], & 
& edgecnc=[6087,6106,6283,6097,6088,6109,6284,6101], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1100),elname="xbrick",eltype="xbrick",typekey=1100) 

        call prepare(lib_xbrick(1101),key=1101, & 
& nodecnc=[782,828,896,907,3354,3400,3468,3479,16726, 16725,17368, 17367,17713, 17714,17102, 17101,16730 & 
& , 16729,17372, 17371,17715, 17716,17106, 17105], & 
& edgecnc=[5791,6112,6285,5979,5793,6114,6286,5981], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1101),elname="xbrick",eltype="xbrick",typekey=1101) 

        call prepare(lib_xbrick(1102),key=1102, & 
& nodecnc=[976,921,875,950,3548,3493,3447,3522,17717, 17718,17366, 17365,17458, 17457,17719, 17720,17721 & 
& , 17722,17370, 17369,17462, 17461,17723, 17724], & 
& edgecnc=[6287,6111,6157,6288,6289,6113,6159,6290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1102),elname="xbrick",eltype="xbrick",typekey=1102) 

        call prepare(lib_xbrick(1103),key=1103, & 
& nodecnc=[925,902,1092,941,3497,3474,3664,3513,17494, 17493,17725, 17726,8686, 8685,17727, 17728,17502 & 
& , 17501,17729, 17730,8694, 8693,17731, 17732], & 
& edgecnc=[6175,6291,1771,6292,6179,6293,1775,6294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1103),elname="xbrick",eltype="xbrick",typekey=1103) 

        call prepare(lib_xbrick(1104),key=1104, & 
& nodecnc=[970,876,931,307,3542,3448,3503,2879,17638, 17637,17300, 17299,12966, 12965,17733, 17734,17644 & 
& , 17643,17304, 17303,12974, 12973,17735, 17736], & 
& edgecnc=[6247,6078,3911,6295,6250,6080,3915,6296], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1104),elname="xbrick",eltype="xbrick",typekey=1104) 

        call prepare(lib_xbrick(1105),key=1105, & 
& nodecnc=[881,2292,989,897,3453,4864,3561,3469,17737, 17738,17739, 17740,6668, 6667,12326, 12325,17741 & 
& , 17742,17743, 17744,6676, 6675,12334, 12333], & 
& edgecnc=[6297,6298,762,3591,6299,6300,766,3595], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1105),elname="xbrick",eltype="xbrick",typekey=1105) 

        call prepare(lib_xbrick(1106),key=1106, & 
& nodecnc=[2281,877,867,999,4853,3449,3439,3571,17745, 17746,16758, 16757,17670, 17669,17747, 17748,17749 & 
& , 17750,16766, 16765,17672, 17671,17751, 17752], & 
& edgecnc=[6301,5807,6263,6302,6303,5811,6264,6304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1106),elname="xbrick",eltype="xbrick",typekey=1106) 

        call prepare(lib_xbrick(1107),key=1107, & 
& nodecnc=[880,905,887,95,3452,3477,3459,2667,17753, 17754,17755, 17756,6458, 6457,17030, 17029,17757 & 
& , 17758,17759, 17760,6466, 6465,17034, 17033], & 
& edgecnc=[6305,6306,657,5943,6307,6308,661,5945], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1107),elname="xbrick",eltype="xbrick",typekey=1107) 

        call prepare(lib_xbrick(1108),key=1108, & 
& nodecnc=[908,928,905,880,3480,3500,3477,3452,17761, 17762,17763, 17764,17754, 17753,17414, 17413,17765 & 
& , 17766,17767, 17768,17758, 17757,17418, 17417], & 
& edgecnc=[6309,6310,6305,6135,6311,6312,6307,6137], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1108),elname="xbrick",eltype="xbrick",typekey=1108) 

        call prepare(lib_xbrick(1109),key=1109, & 
& nodecnc=[2292,881,845,843,4864,3453,3417,3415,17738, 17737,17510, 17509,17162, 17161,17769, 17770,17742 & 
& , 17741,17512, 17511,17168, 17167,17771, 17772], & 
& edgecnc=[6297,6183,6009,6313,6299,6184,6012,6314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1109),elname="xbrick",eltype="xbrick",typekey=1109) 

        call prepare(lib_xbrick(1110),key=1110, & 
& nodecnc=[979,889,275,2224,3551,3461,2847,4796,17773, 17774,6610, 6609,12322, 12321,17775, 17776,17777 & 
& , 17778,6618, 6617,12330, 12329,17779, 17780], & 
& edgecnc=[6315,733,3589,6316,6317,737,3593,6318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1110),elname="xbrick",eltype="xbrick",typekey=1110) 

        call prepare(lib_xbrick(1111),key=1111, & 
& nodecnc=[884,915,2538,882,3456,3487,5110,3454,17781, 17782,17558, 17557,8314, 8313,17230, 17229,17783 & 
& , 17784,17564, 17563,8322, 8321,17236, 17235], & 
& edgecnc=[6319,6207,1585,6043,6320,6210,1589,6046], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1111),elname="xbrick",eltype="xbrick",typekey=1111) 

        call prepare(lib_xbrick(1112),key=1112, & 
& nodecnc=[888,932,950,883,3460,3504,3522,3455,17785, 17786,17787, 17788,17460, 17459,17610, 17609,17789 & 
& , 17790,17791, 17792,17464, 17463,17616, 17615], & 
& edgecnc=[6321,6322,6158,6233,6323,6324,6160,6236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1112),elname="xbrick",eltype="xbrick",typekey=1112) 

        call prepare(lib_xbrick(1113),key=1113, & 
& nodecnc=[964,884,919,939,3536,3456,3491,3511,17793, 17794,17228, 17227,17795, 17796,17797, 17798,17799 & 
& , 17800,17234, 17233,17801, 17802,17803, 17804], & 
& edgecnc=[6325,6042,6326,6327,6328,6045,6329,6330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1113),elname="xbrick",eltype="xbrick",typekey=1113) 

        call prepare(lib_xbrick(1114),key=1114, & 
& nodecnc=[962,915,884,964,3534,3487,3456,3536,17805, 17806,17782, 17781,17794, 17793,17807, 17808,17809 & 
& , 17810,17784, 17783,17800, 17799,17811, 17812], & 
& edgecnc=[6331,6319,6325,6332,6333,6320,6328,6334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1114),elname="xbrick",eltype="xbrick",typekey=1114) 

        call prepare(lib_xbrick(1115),key=1115, & 
& nodecnc=[846,937,175,885,3418,3509,2747,3457,17522, 17521,17514, 17513,17813, 17814,17815, 17816,17526 & 
& , 17525,17518, 17517,17817, 17818,17819, 17820], & 
& edgecnc=[6189,6185,6335,6336,6191,6187,6337,6338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1115),elname="xbrick",eltype="xbrick",typekey=1115) 

        call prepare(lib_xbrick(1116),key=1116, & 
& nodecnc=[885,2409,823,846,3457,4981,3395,3418,17700, 17699,17202, 17201,16938, 16937,17816, 17815,17706 & 
& , 17705,17208, 17207,16944, 16943,17820, 17819], & 
& edgecnc=[6278,6029,5897,6336,6281,6032,5900,6338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1116),elname="xbrick",eltype="xbrick",typekey=1116) 

        call prepare(lib_xbrick(1117),key=1117, & 
& nodecnc=[122,886,930,922,2694,3458,3502,3494,17046, 17045,17821, 17822,17823, 17824,17426, 17425,17052 & 
& , 17051,17825, 17826,17827, 17828,17434, 17433], & 
& edgecnc=[5951,6339,6340,6141,5954,6341,6342,6145], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1117),elname="xbrick",eltype="xbrick",typekey=1117) 

        call prepare(lib_xbrick(1118),key=1118, & 
& nodecnc=[969,1009,1017,917,3541,3581,3589,3489,9838, 9837,17829, 17830,17831, 17832,17833, 17834,9844 & 
& , 9843,17835, 17836,17837, 17838,17839, 17840], & 
& edgecnc=[2347,6343,6344,6345,2350,6346,6347,6348], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1118),elname="xbrick",eltype="xbrick",typekey=1118) 

        call prepare(lib_xbrick(1119),key=1119, & 
& nodecnc=[959,887,905,917,3531,3459,3477,3489,17686, 17685,17756, 17755,17841, 17842,17843, 17844,17690 & 
& , 17689,17760, 17759,17845, 17846,17847, 17848], & 
& edgecnc=[6271,6306,6349,6350,6273,6308,6351,6352], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1119),elname="xbrick",eltype="xbrick",typekey=1119) 

        call prepare(lib_xbrick(1120),key=1120, & 
& nodecnc=[943,957,932,923,3515,3529,3504,3495,17849, 17850,17851, 17852,17853, 17854,17855, 17856,17857 & 
& , 17858,17859, 17860,17861, 17862,17863, 17864], & 
& edgecnc=[6353,6354,6355,6356,6357,6358,6359,6360], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1120),elname="xbrick",eltype="xbrick",typekey=1120) 

        call prepare(lib_xbrick(1121),key=1121, & 
& nodecnc=[1097,899,852,895,3669,3471,3424,3467,17865, 17866,17286, 17285,17382, 17381,17867, 17868,17869 & 
& , 17870,17292, 17291,17384, 17383,17871, 17872], & 
& edgecnc=[6361,6071,6119,6362,6363,6074,6120,6364], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1121),elname="xbrick",eltype="xbrick",typekey=1121) 

        call prepare(lib_xbrick(1122),key=1122, & 
& nodecnc=[891,931,857,842,3463,3503,3429,3414,17598, 17597,17298, 17297,17294, 17293,17488, 17487,17602 & 
& , 17601,17302, 17301,17296, 17295,17492, 17491], & 
& edgecnc=[6227,6077,6075,6172,6229,6079,6076,6174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1122),elname="xbrick",eltype="xbrick",typekey=1122) 

        call prepare(lib_xbrick(1123),key=1123, & 
& nodecnc=[892,2536,914,2478,3464,5108,3486,5050,11848, 11847,17873, 17874,17875, 17876,17354, 17353,11856 & 
& , 11855,17877, 17878,17879, 17880,17360, 17359], & 
& edgecnc=[3352,6365,6366,6105,3356,6367,6368,6108], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1123),elname="xbrick",eltype="xbrick",typekey=1123) 

        call prepare(lib_xbrick(1124),key=1124, & 
& nodecnc=[865,2429,929,2378,3437,5001,3501,4950,17881, 17882,17883, 17884,17885, 17886,17014, 17013,17887 & 
& , 17888,17889, 17890,17891, 17892,17022, 17021], & 
& edgecnc=[6369,6370,6371,5935,6372,6373,6374,5939], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1124),elname="xbrick",eltype="xbrick",typekey=1124) 

        call prepare(lib_xbrick(1125),key=1125, & 
& nodecnc=[951,929,2429,903,3523,3501,5001,3475,17893, 17894,17884, 17883,17396, 17395,17656, 17655,17895 & 
& , 17896,17890, 17889,17402, 17401,17664, 17663], & 
& edgecnc=[6375,6370,6126,6256,6376,6373,6129,6260], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1125),elname="xbrick",eltype="xbrick",typekey=1125) 

        call prepare(lib_xbrick(1126),key=1126, & 
& nodecnc=[942,978,907,896,3514,3550,3479,3468,17897, 17898,17192, 17191,17714, 17713,11166, 11165,17899 & 
& , 17900,17196, 17195,17716, 17715,11172, 11171], & 
& edgecnc=[6377,6024,6285,3011,6378,6026,6286,3014], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1126),elname="xbrick",eltype="xbrick",typekey=1126) 

        call prepare(lib_xbrick(1127),key=1127, & 
& nodecnc=[1052,1067,1032,997,3624,3639,3604,3569,17901, 17902,17903, 17904,6672, 6671,17905, 17906,17907 & 
& , 17908,17909, 17910,6680, 6679,17911, 17912], & 
& edgecnc=[6379,6380,764,6381,6382,6383,768,6384], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1127),elname="xbrick",eltype="xbrick",typekey=1127) 

        call prepare(lib_xbrick(1128),key=1128, & 
& nodecnc=[2281,999,1052,997,4853,3571,3624,3569,17748, 17747,17913, 17914,17906, 17905,17915, 17916,17752 & 
& , 17751,17917, 17918,17912, 17911,17919, 17920], & 
& edgecnc=[6302,6385,6381,6386,6304,6387,6384,6388], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1128),elname="xbrick",eltype="xbrick",typekey=1128) 

        call prepare(lib_xbrick(1129),key=1129, & 
& nodecnc=[901,898,926,934,3473,3470,3498,3506,17530, 17529,17310, 17309,17921, 17922,17923, 17924,17536 & 
& , 17535,17316, 17315,17925, 17926,17927, 17928], & 
& edgecnc=[6193,6083,6389,6390,6196,6086,6391,6392], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1129),elname="xbrick",eltype="xbrick",typekey=1129) 

        call prepare(lib_xbrick(1130),key=1130, & 
& nodecnc=[891,899,1097,920,3463,3471,3669,3492,17486, 17485,17866, 17865,17929, 17930,17600, 17599,17490 & 
& , 17489,17870, 17869,17931, 17932,17604, 17603], & 
& edgecnc=[6171,6361,6393,6228,6173,6363,6394,6230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1130),elname="xbrick",eltype="xbrick",typekey=1130) 

        call prepare(lib_xbrick(1131),key=1131, & 
& nodecnc=[2334,855,911,924,4906,3427,3483,3496,17096, 17095,17238, 17237,17933, 17934,17058, 17057,17100 & 
& , 17099,17242, 17241,17935, 17936,17066, 17065], & 
& edgecnc=[5976,6047,6395,5957,5978,6049,6396,5961], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1131),elname="xbrick",eltype="xbrick",typekey=1131) 

        call prepare(lib_xbrick(1132),key=1132, & 
& nodecnc=[961,206,901,934,3533,2778,3473,3506,17937, 17938,17939, 17940,17924, 17923,17941, 17942,17943 & 
& , 17944,17945, 17946,17928, 17927,17947, 17948], & 
& edgecnc=[6397,6398,6390,6399,6400,6401,6392,6402], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1132),elname="xbrick",eltype="xbrick",typekey=1132) 

        call prepare(lib_xbrick(1133),key=1133, & 
& nodecnc=[925,940,890,863,3497,3512,3462,3435,17949, 17950,17951, 17952,17544, 17543,17496, 17495,17953 & 
& , 17954,17955, 17956,17548, 17547,17504, 17503], & 
& edgecnc=[6403,6404,6200,6176,6405,6406,6202,6180], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1133),elname="xbrick",eltype="xbrick",typekey=1133) 

        call prepare(lib_xbrick(1134),key=1134, & 
& nodecnc=[276,1021,926,904,2848,3593,3498,3476,17957, 17958,17959, 17960,17308, 17307,6550, 6549,17961 & 
& , 17962,17963, 17964,17314, 17313,6558, 6557], & 
& edgecnc=[6407,6408,6082,703,6409,6410,6085,707], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1134),elname="xbrick",eltype="xbrick",typekey=1134) 

        call prepare(lib_xbrick(1135),key=1135, & 
& nodecnc=[306,984,990,940,2878,3556,3562,3512,8594, 8593,17965, 17966,17967, 17968,17969, 17970,8602 & 
& , 8601,17971, 17972,17973, 17974,17975, 17976], & 
& edgecnc=[1725,6411,6412,6413,1729,6414,6415,6416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1135),elname="xbrick",eltype="xbrick",typekey=1135) 

        call prepare(lib_xbrick(1136),key=1136, & 
& nodecnc=[943,923,906,990,3515,3495,3478,3562,17856, 17855,17977, 17978,17979, 17980,17981, 17982,17864 & 
& , 17863,17983, 17984,17985, 17986,17987, 17988], & 
& edgecnc=[6356,6417,6418,6419,6360,6420,6421,6422], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1136),elname="xbrick",eltype="xbrick",typekey=1136) 

        call prepare(lib_xbrick(1137),key=1137, & 
& nodecnc=[1033,96,945,2531,3605,2668,3517,5103,9842, 9841,17989, 17990,17991, 17992,17993, 17994,9848 & 
& , 9847,17995, 17996,17997, 17998,17999, 18000], & 
& edgecnc=[2349,6423,6424,6425,2352,6426,6427,6428], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1137),elname="xbrick",eltype="xbrick",typekey=1137) 

        call prepare(lib_xbrick(1138),key=1138, & 
& nodecnc=[2466,910,872,909,5038,3482,3444,3481,18001, 18002,16928, 16927,18003, 18004,18005, 18006,18007 & 
& , 18008,16936, 16935,18009, 18010,18011, 18012], & 
& edgecnc=[6429,5892,6430,6431,6432,5896,6433,6434], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1138),elname="xbrick",eltype="xbrick",typekey=1138) 

        call prepare(lib_xbrick(1139),key=1139, & 
& nodecnc=[994,1000,960,933,3566,3572,3532,3505,18013, 18014,8830, 8829,18015, 18016,17624, 17623,18017 & 
& , 18018,8838, 8837,18019, 18020,17632, 17631], & 
& edgecnc=[6435,1843,6436,6240,6437,1847,6438,6244], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1139),elname="xbrick",eltype="xbrick",typekey=1139) 

        call prepare(lib_xbrick(1140),key=1140, & 
& nodecnc=[910,2419,911,869,3482,4991,3483,3441,18021, 18022,18023, 18024,17240, 17239,16922, 16921,18025 & 
& , 18026,18027, 18028,17244, 17243,16930, 16929], & 
& edgecnc=[6439,6440,6048,5889,6441,6442,6050,5893], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1140),elname="xbrick",eltype="xbrick",typekey=1140) 

        call prepare(lib_xbrick(1141),key=1141, & 
& nodecnc=[2497,933,2457,2466,5069,3505,5029,5038,17618, 17617,18029, 18030,18031, 18032,18033, 18034 & 
& ,17626, 17625,18035, 18036,18037, 18038,18039, 18040], & 
& edgecnc=[6237,6443,6444,6445,6241,6446,6447,6448], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1141),elname="xbrick",eltype="xbrick",typekey=1141) 

        call prepare(lib_xbrick(1142),key=1142, & 
& nodecnc=[2473,912,901,206,5045,3484,3473,2778,16714, 16713,17532, 17531,17940, 17939,10380, 10379,16722 & 
& , 16721,17538, 17537,17946, 17945,10386, 10385], & 
& edgecnc=[5785,6194,6398,2618,5789,6197,6401,2621], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1142),elname="xbrick",eltype="xbrick",typekey=1142) 

        call prepare(lib_xbrick(1143),key=1143, & 
& nodecnc=[886,2511,853,930,3458,5083,3425,3502,17568, 17567,18041, 18042,18043, 18044,17822, 17821,17572 & 
& , 17571,18045, 18046,18047, 18048,17826, 17825], & 
& edgecnc=[6212,6449,6450,6339,6214,6451,6452,6341], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1143),elname="xbrick",eltype="xbrick",typekey=1143) 

        call prepare(lib_xbrick(1144),key=1144, & 
& nodecnc=[2549,2536,954,986,5121,5108,3526,3558,18049, 18050,11846, 11845,18051, 18052,18053, 18054,18055 & 
& , 18056,11854, 11853,18057, 18058,18059, 18060], & 
& edgecnc=[6453,3351,6454,6455,6456,3355,6457,6458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1144),elname="xbrick",eltype="xbrick",typekey=1144) 

        call prepare(lib_xbrick(1145),key=1145, & 
& nodecnc=[914,2527,871,2478,3486,5099,3443,5050,18061, 18062,17340, 17339,17710, 17709,17876, 17875,18063 & 
& , 18064,17348, 17347,17712, 17711,17880, 17879], & 
& edgecnc=[6459,6098,6283,6366,6460,6102,6284,6368], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1145),elname="xbrick",eltype="xbrick",typekey=1145) 

        call prepare(lib_xbrick(1146),key=1146, & 
& nodecnc=[2479,953,954,913,5051,3525,3526,3485,18065, 18066,18067, 18068,11852, 11851,17698, 17697,18069 & 
& , 18070,18071, 18072,11860, 11859,17704, 17703], & 
& edgecnc=[6461,6462,3354,6277,6463,6464,3358,6280], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1146),elname="xbrick",eltype="xbrick",typekey=1146) 

        call prepare(lib_xbrick(1147),key=1147, & 
& nodecnc=[2561,1015,962,2556,5133,3587,3534,5128,18073, 18074,18075, 18076,18077, 18078,18079, 18080 & 
& ,18081, 18082,18083, 18084,18085, 18086,18087, 18088], & 
& edgecnc=[6465,6466,6467,6468,6469,6470,6471,6472], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1147),elname="xbrick",eltype="xbrick",typekey=1147) 

        call prepare(lib_xbrick(1148),key=1148, & 
& nodecnc=[956,209,916,2517,3528,2781,3488,5089,18089, 18090,18091, 18092,17660, 17659,17574, 17573,18093 & 
& , 18094,18095, 18096,17668, 17667,17582, 17581], & 
& edgecnc=[6473,6474,6258,6215,6475,6476,6262,6219], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1148),elname="xbrick",eltype="xbrick",typekey=1148) 

        call prepare(lib_xbrick(1149),key=1149, & 
& nodecnc=[980,175,918,938,3552,2747,3490,3510,18097, 18098,17516, 17515,17556, 17555,18099, 18100,18101 & 
& , 18102,17520, 17519,17562, 17561,18103, 18104], & 
& edgecnc=[6477,6186,6206,6478,6479,6188,6209,6480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1149),elname="xbrick",eltype="xbrick",typekey=1149) 

        call prepare(lib_xbrick(1150),key=1150, & 
& nodecnc=[939,919,958,158,3511,3491,3530,2730,17796, 17795,17550, 17549,18105, 18106,18107, 18108,17802 & 
& , 17801,17552, 17551,18109, 18110,18111, 18112], & 
& edgecnc=[6326,6203,6481,6482,6329,6204,6483,6484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1150),elname="xbrick",eltype="xbrick",typekey=1150) 

        call prepare(lib_xbrick(1151),key=1151, & 
& nodecnc=[1057,920,1097,308,3629,3492,3669,2880,8742, 8741,17930, 17929,18113, 18114,18115, 18116,8750 & 
& , 8749,17932, 17931,18117, 18118,18119, 18120], & 
& edgecnc=[1799,6393,6485,6486,1803,6394,6487,6488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1151),elname="xbrick",eltype="xbrick",typekey=1151) 

        call prepare(lib_xbrick(1152),key=1152, & 
& nodecnc=[976,2476,966,921,3548,5048,3538,3493,18121, 18122,18123, 18124,11164, 11163,17718, 17717,18125 & 
& , 18126,18127, 18128,11170, 11169,17722, 17721], & 
& edgecnc=[6489,6490,3010,6287,6491,6492,3013,6289], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1152),elname="xbrick",eltype="xbrick",typekey=1152) 

        call prepare(lib_xbrick(1153),key=1153, & 
& nodecnc=[972,927,946,967,3544,3499,3518,3539,18129, 18130,17440, 17439,17422, 17421,18131, 18132,18133 & 
& , 18134,17446, 17445,17430, 17429,18135, 18136], & 
& edgecnc=[6493,6148,6139,6494,6495,6151,6143,6496], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1153),elname="xbrick",eltype="xbrick",typekey=1153) 

        call prepare(lib_xbrick(1154),key=1154, & 
& nodecnc=[1048,2423,983,1037,3620,4995,3555,3609,6410, 6409,18137, 18138,6394, 6393,18139, 18140,6418 & 
& , 6417,18141, 18142,6402, 6401,18143, 18144], & 
& edgecnc=[633,6497,625,6498,637,6499,629,6500], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1154),elname="xbrick",eltype="xbrick",typekey=1154) 

        call prepare(lib_xbrick(1155),key=1155, & 
& nodecnc=[2435,923,932,888,5007,3495,3504,3460,18145, 18146,17854, 17853,17786, 17785,17608, 17607,18147 & 
& , 18148,17862, 17861,17790, 17789,17614, 17613], & 
& edgecnc=[6501,6355,6321,6232,6502,6359,6323,6235], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1155),elname="xbrick",eltype="xbrick",typekey=1155) 

        call prepare(lib_xbrick(1156),key=1156, & 
& nodecnc=[2419,935,924,911,4991,3507,3496,3483,18149, 18150,18151, 18152,17934, 17933,18024, 18023,18153 & 
& , 18154,18155, 18156,17936, 17935,18028, 18027], & 
& edgecnc=[6503,6504,6395,6440,6505,6506,6396,6442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1156),elname="xbrick",eltype="xbrick",typekey=1156) 

        call prepare(lib_xbrick(1157),key=1157, & 
& nodecnc=[940,925,941,306,3512,3497,3513,2878,17950, 17949,17728, 17727,18157, 18158,17970, 17969,17954 & 
& , 17953,17732, 17731,18159, 18160,17976, 17975], & 
& edgecnc=[6403,6292,6507,6413,6405,6294,6508,6416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1157),elname="xbrick",eltype="xbrick",typekey=1157) 

        call prepare(lib_xbrick(1158),key=1158, & 
& nodecnc=[1051,1021,2283,1053,3623,3593,4855,3625,18161, 18162,18163, 18164,6534, 6533,18165, 18166,18167 & 
& , 18168,18169, 18170,6542, 6541,18171, 18172], & 
& edgecnc=[6509,6510,695,6511,6512,6513,699,6514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1158),elname="xbrick",eltype="xbrick",typekey=1158) 

        call prepare(lib_xbrick(1159),key=1159, & 
& nodecnc=[1003,952,927,972,3575,3524,3499,3544,18173, 18174,17442, 17441,18130, 18129,18175, 18176,18177 & 
& , 18178,17448, 17447,18134, 18133,18179, 18180], & 
& edgecnc=[6515,6149,6493,6516,6517,6152,6495,6518], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1159),elname="xbrick",eltype="xbrick",typekey=1159) 

        call prepare(lib_xbrick(1160),key=1160, & 
& nodecnc=[969,917,905,928,3541,3489,3477,3500,17834, 17833,17842, 17841,17764, 17763,18181, 18182,17840 & 
& , 17839,17846, 17845,17768, 17767,18183, 18184], & 
& edgecnc=[6345,6349,6310,6519,6348,6351,6312,6520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1160),elname="xbrick",eltype="xbrick",typekey=1160) 

        call prepare(lib_xbrick(1161),key=1161, & 
& nodecnc=[96,969,928,945,2668,3541,3500,3517,9840, 9839,18182, 18181,18185, 18186,17990, 17989,9846, 9845 & 
& ,18184, 18183,18187, 18188,17996, 17995], & 
& edgecnc=[2348,6519,6521,6423,2351,6520,6522,6426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1161),elname="xbrick",eltype="xbrick",typekey=1161) 

        call prepare(lib_xbrick(1162),key=1162, & 
& nodecnc=[308,1097,895,948,2880,3669,3467,3520,18114, 18113,17868, 17867,11238, 11237,18189, 18190,18118 & 
& , 18117,17872, 17871,11246, 11245,18191, 18192], & 
& edgecnc=[6485,6362,3047,6523,6487,6364,3051,6524], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1162),elname="xbrick",eltype="xbrick",typekey=1162) 

        call prepare(lib_xbrick(1163),key=1163, & 
& nodecnc=[2412,936,924,935,4984,3508,3496,3507,18193, 18194,17060, 17059,18152, 18151,18195, 18196,18197 & 
& , 18198,17068, 17067,18156, 18155,18199, 18200], & 
& edgecnc=[6525,5958,6504,6526,6527,5962,6506,6528], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1163),elname="xbrick",eltype="xbrick",typekey=1163) 

        call prepare(lib_xbrick(1164),key=1164, & 
& nodecnc=[974,949,2378,929,3546,3521,4950,3501,18201, 18202,18203, 18204,17886, 17885,18205, 18206,18207 & 
& , 18208,18209, 18210,17892, 17891,18211, 18212], & 
& edgecnc=[6529,6530,6371,6531,6532,6533,6374,6534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1164),elname="xbrick",eltype="xbrick",typekey=1164) 

        call prepare(lib_xbrick(1165),key=1165, & 
& nodecnc=[988,930,853,2473,3560,3502,3425,5045,18213, 18214,18044, 18043,16716, 16715,10378, 10377,18215 & 
& , 18216,18048, 18047,16724, 16723,10384, 10383], & 
& edgecnc=[6535,6450,5786,2617,6536,6452,5790,2620], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1165),elname="xbrick",eltype="xbrick",typekey=1165) 

        call prepare(lib_xbrick(1166),key=1166, & 
& nodecnc=[970,307,955,975,3542,2879,3527,3547,17734, 17733,12972, 12971,18217, 18218,18219, 18220,17736 & 
& , 17735,12980, 12979,18221, 18222,18223, 18224], & 
& edgecnc=[6295,3914,6537,6538,6296,3918,6539,6540], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1166),elname="xbrick",eltype="xbrick",typekey=1166) 

        call prepare(lib_xbrick(1167),key=1167, & 
& nodecnc=[137,950,932,957,2709,3522,3504,3529,18225, 18226,17788, 17787,17852, 17851,18227, 18228,18229 & 
& , 18230,17792, 17791,17860, 17859,18231, 18232], & 
& edgecnc=[6541,6322,6354,6542,6543,6324,6358,6544], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1167),elname="xbrick",eltype="xbrick",typekey=1167) 

        call prepare(lib_xbrick(1168),key=1168, & 
& nodecnc=[141,2457,933,960,2713,5029,3505,3532,18233, 18234,18030, 18029,18016, 18015,18235, 18236,18237 & 
& , 18238,18036, 18035,18020, 18019,18239, 18240], & 
& edgecnc=[6545,6443,6436,6546,6547,6446,6438,6548], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1168),elname="xbrick",eltype="xbrick",typekey=1168) 

        call prepare(lib_xbrick(1169),key=1169, & 
& nodecnc=[982,961,934,1051,3554,3533,3506,3623,18241, 18242,17942, 17941,18243, 18244,18245, 18246,18247 & 
& , 18248,17948, 17947,18249, 18250,18251, 18252], & 
& edgecnc=[6549,6399,6550,6551,6552,6402,6553,6554], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1169),elname="xbrick",eltype="xbrick",typekey=1169) 

        call prepare(lib_xbrick(1170),key=1170, & 
& nodecnc=[968,936,2412,996,3540,3508,4984,3568,18253, 18254,18194, 18193,5540, 5539,18255, 18256,18257 & 
& , 18258,18198, 18197,5548, 5547,18259, 18260], & 
& edgecnc=[6555,6525,198,6556,6557,6527,202,6558], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1170),elname="xbrick",eltype="xbrick",typekey=1170) 

        call prepare(lib_xbrick(1171),key=1171, & 
& nodecnc=[935,2419,2457,141,3507,4991,5029,2713,18150, 18149,18261, 18262,18234, 18233,18263, 18264,18154 & 
& , 18153,18265, 18266,18238, 18237,18267, 18268], & 
& edgecnc=[6503,6559,6545,6560,6505,6561,6547,6562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1171),elname="xbrick",eltype="xbrick",typekey=1171) 

        call prepare(lib_xbrick(1172),key=1172, & 
& nodecnc=[885,175,980,2479,3457,2747,3552,5051,17814, 17813,18098, 18097,18269, 18270,17702, 17701,17818 & 
& , 17817,18102, 18101,18271, 18272,17708, 17707], & 
& edgecnc=[6335,6477,6563,6279,6337,6479,6564,6282], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1172),elname="xbrick",eltype="xbrick",typekey=1172) 

        call prepare(lib_xbrick(1173),key=1173, & 
& nodecnc=[939,158,2438,985,3511,2730,5010,3557,18108, 18107,18273, 18274,18275, 18276,18277, 18278,18112 & 
& , 18111,18279, 18280,18281, 18282,18283, 18284], & 
& edgecnc=[6482,6565,6566,6567,6484,6568,6569,6570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1173),elname="xbrick",eltype="xbrick",typekey=1173) 

        call prepare(lib_xbrick(1174),key=1174, & 
& nodecnc=[940,990,906,890,3512,3562,3478,3462,17968, 17967,17980, 17979,17322, 17321,17952, 17951,17974 & 
& , 17973,17986, 17985,17326, 17325,17956, 17955], & 
& edgecnc=[6412,6418,6089,6404,6415,6421,6091,6406], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1174),elname="xbrick",eltype="xbrick",typekey=1174) 

        call prepare(lib_xbrick(1175),key=1175, & 
& nodecnc=[1010,306,941,987,3582,2878,3513,3559,8596, 8595,18158, 18157,8684, 8683,18285, 18286,8604, 8603 & 
& ,18160, 18159,8692, 8691,18287, 18288], & 
& edgecnc=[1726,6507,1770,6571,1730,6508,1774,6572], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1175),elname="xbrick",eltype="xbrick",typekey=1175) 

        call prepare(lib_xbrick(1176),key=1176, & 
& nodecnc=[1072,1068,987,1025,3644,3640,3559,3597,13774, 13773,18289, 18290,8682, 8681,18291, 18292,13780 & 
& , 13779,18293, 18294,8690, 8689,18295, 18296], & 
& edgecnc=[4315,6573,1769,6574,4318,6575,1773,6576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1176),elname="xbrick",eltype="xbrick",typekey=1176) 

        call prepare(lib_xbrick(1177),key=1177, & 
& nodecnc=[1031,136,1082,2477,3603,2708,3654,5049,18297, 18298,18299, 18300,8502, 8501,18301, 18302,18303 & 
& , 18304,18305, 18306,8510, 8509,18307, 18308], & 
& edgecnc=[6577,6578,1679,6579,6580,6581,1683,6582], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1177),elname="xbrick",eltype="xbrick",typekey=1177) 

        call prepare(lib_xbrick(1178),key=1178, & 
& nodecnc=[957,943,1047,981,3529,3515,3619,3553,17850, 17849,18309, 18310,18311, 18312,18313, 18314,17858 & 
& , 17857,18315, 18316,18317, 18318,18319, 18320], & 
& edgecnc=[6353,6583,6584,6585,6357,6586,6587,6588], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1178),elname="xbrick",eltype="xbrick",typekey=1178) 

        call prepare(lib_xbrick(1179),key=1179, & 
& nodecnc=[889,979,1014,944,3461,3551,3586,3516,17774, 17773,18321, 18322,18323, 18324,18325, 18326,17778 & 
& , 17777,18327, 18328,18329, 18330,18331, 18332], & 
& edgecnc=[6315,6589,6590,6591,6317,6592,6593,6594], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1179),elname="xbrick",eltype="xbrick",typekey=1179) 

        call prepare(lib_xbrick(1180),key=1180, & 
& nodecnc=[878,889,944,947,3450,3461,3516,3519,6612, 6611,18326, 18325,12338, 12337,17478, 17477,6620 & 
& , 6619,18332, 18331,12346, 12345,17482, 17481], & 
& edgecnc=[734,6591,3597,6167,738,6594,3601,6169], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1180),elname="xbrick",eltype="xbrick",typekey=1180) 

        call prepare(lib_xbrick(1181),key=1181, & 
& nodecnc=[908,2495,945,928,3480,5067,3517,3500,17676, 17675,18333, 18334,18186, 18185,17762, 17761,17680 & 
& , 17679,18335, 18336,18188, 18187,17766, 17765], & 
& edgecnc=[6266,6595,6521,6309,6268,6596,6522,6311], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1181),elname="xbrick",eltype="xbrick",typekey=1181) 

        call prepare(lib_xbrick(1182),key=1182, & 
& nodecnc=[952,1003,965,2495,3524,3575,3537,5067,18174, 18173,10370, 10369,18337, 18338,17674, 17673,18178 & 
& , 18177,10376, 10375,18339, 18340,17678, 17677], & 
& edgecnc=[6515,2613,6597,6265,6517,2616,6598,6267], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1182),elname="xbrick",eltype="xbrick",typekey=1182) 

        call prepare(lib_xbrick(1183),key=1183, & 
& nodecnc=[968,1001,1108,948,3540,3573,3680,3520,18341, 18342,11326, 11325,18343, 18344,18345, 18346,18347 & 
& , 18348,11334, 11333,18349, 18350,18351, 18352], & 
& edgecnc=[6599,3091,6600,6601,6602,3095,6603,6604], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1183),elname="xbrick",eltype="xbrick",typekey=1183) 

        call prepare(lib_xbrick(1184),key=1184, & 
& nodecnc=[1078,308,948,1108,3650,2880,3520,3680,18353, 18354,18190, 18189,18344, 18343,11332, 11331,18355 & 
& , 18356,18192, 18191,18350, 18349,11340, 11339], & 
& edgecnc=[6605,6523,6600,3094,6606,6524,6603,3098], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1184),elname="xbrick",eltype="xbrick",typekey=1184) 

        call prepare(lib_xbrick(1185),key=1185, & 
& nodecnc=[2378,949,1030,274,4950,3521,3602,2846,18204, 18203,12310, 12309,8364, 8363,17016, 17015,18210 & 
& , 18209,12318, 12317,8372, 8371,17024, 17023], & 
& edgecnc=[6530,3583,1610,5936,6533,3587,1614,5940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1185),elname="xbrick",eltype="xbrick",typekey=1185) 

        call prepare(lib_xbrick(1186),key=1186, & 
& nodecnc=[2444,974,929,951,5016,3546,3501,3523,18357, 18358,18206, 18205,17894, 17893,18359, 18360,18361 & 
& , 18362,18212, 18211,17896, 17895,18363, 18364], & 
& edgecnc=[6607,6531,6375,6608,6609,6534,6376,6610], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1186),elname="xbrick",eltype="xbrick",typekey=1186) 

        call prepare(lib_xbrick(1187),key=1187, & 
& nodecnc=[1004,2379,1029,2444,3576,4951,3601,5016,18365, 18366,18367, 18368,18369, 18370,18371, 18372 & 
& ,18373, 18374,18375, 18376,18377, 18378,18379, 18380], & 
& edgecnc=[6611,6612,6613,6614,6615,6616,6617,6618], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1187),elname="xbrick",eltype="xbrick",typekey=1187) 

        call prepare(lib_xbrick(1188),key=1188, & 
& nodecnc=[2444,951,916,1004,5016,3523,3488,3576,18360, 18359,17654, 17653,18381, 18382,18372, 18371,18364 & 
& , 18363,17662, 17661,18383, 18384,18380, 18379], & 
& edgecnc=[6608,6255,6619,6614,6610,6259,6620,6618], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1188),elname="xbrick",eltype="xbrick",typekey=1188) 

        call prepare(lib_xbrick(1189),key=1189, & 
& nodecnc=[2537,953,2479,980,5109,3525,5051,3552,18385, 18386,18066, 18065,18270, 18269,18387, 18388,18389 & 
& , 18390,18070, 18069,18272, 18271,18391, 18392], & 
& edgecnc=[6621,6461,6563,6622,6623,6463,6564,6624], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1189),elname="xbrick",eltype="xbrick",typekey=1189) 

        call prepare(lib_xbrick(1190),key=1190, & 
& nodecnc=[2544,914,963,956,5116,3486,3535,3528,18393, 18394,18395, 18396,18397, 18398,17580, 17579,18399 & 
& , 18400,18401, 18402,18403, 18404,17588, 17587], & 
& edgecnc=[6625,6626,6627,6218,6628,6629,6630,6222], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1190),elname="xbrick",eltype="xbrick",typekey=1190) 

        call prepare(lib_xbrick(1191),key=1191, & 
& nodecnc=[914,2536,2549,963,3486,5108,5121,3535,17874, 17873,18050, 18049,18405, 18406,18396, 18395,17878 & 
& , 17877,18056, 18055,18407, 18408,18402, 18401], & 
& edgecnc=[6365,6453,6631,6626,6367,6456,6632,6629], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1191),elname="xbrick",eltype="xbrick",typekey=1191) 

        call prepare(lib_xbrick(1192),key=1192, & 
& nodecnc=[1035,975,955,1008,3607,3547,3527,3580,12954, 12953,18218, 18217,18409, 18410,18411, 18412,12960 & 
& , 12959,18222, 18221,18413, 18414,18415, 18416], & 
& edgecnc=[3905,6537,6633,6634,3908,6539,6635,6636], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1192),elname="xbrick",eltype="xbrick",typekey=1192) 

        call prepare(lib_xbrick(1193),key=1193, & 
& nodecnc=[955,1050,1034,1008,3527,3622,3606,3580,12970, 12969,8746, 8745,18417, 18418,18410, 18409,12978 & 
& , 12977,8754, 8753,18419, 18420,18414, 18413], & 
& edgecnc=[3913,1801,6637,6633,3917,1805,6638,6635], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1193),elname="xbrick",eltype="xbrick",typekey=1193) 

        call prepare(lib_xbrick(1194),key=1194, & 
& nodecnc=[963,1005,209,956,3535,3577,2781,3528,18421, 18422,8454, 8453,18090, 18089,18398, 18397,18423 & 
& , 18424,8462, 8461,18094, 18093,18404, 18403], & 
& edgecnc=[6639,1655,6473,6627,6640,1659,6475,6630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1194),elname="xbrick",eltype="xbrick",typekey=1194) 

        call prepare(lib_xbrick(1195),key=1195, & 
& nodecnc=[137,957,981,1006,2709,3529,3553,3578,18228, 18227,18314, 18313,18425, 18426,18427, 18428,18232 & 
& , 18231,18320, 18319,18429, 18430,18431, 18432], & 
& edgecnc=[6542,6585,6641,6642,6544,6588,6643,6644], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1195),elname="xbrick",eltype="xbrick",typekey=1195) 

        call prepare(lib_xbrick(1196),key=1196, & 
& nodecnc=[868,978,158,958,3440,3550,2730,3530,17190, 17189,18433, 18434,18106, 18105,17376, 17375,17194 & 
& , 17193,18435, 18436,18110, 18109,17380, 17379], & 
& edgecnc=[6023,6645,6481,6116,6025,6646,6483,6118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1196),elname="xbrick",eltype="xbrick",typekey=1196) 

        call prepare(lib_xbrick(1197),key=1197, & 
& nodecnc=[2466,909,959,2497,5038,3481,3531,5069,18006, 18005,17688, 17687,17620, 17619,18034, 18033,18012 & 
& , 18011,17692, 17691,17628, 17627,18040, 18039], & 
& edgecnc=[6431,6272,6238,6445,6434,6274,6242,6448], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1197),elname="xbrick",eltype="xbrick",typekey=1197) 

        call prepare(lib_xbrick(1198),key=1198, & 
& nodecnc=[141,960,973,971,2713,3532,3545,3543,18236, 18235,8836, 8835,18437, 18438,18439, 18440,18240 & 
& , 18239,8844, 8843,18441, 18442,18443, 18444], & 
& edgecnc=[6546,1846,6647,6648,6548,1850,6649,6650], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1198),elname="xbrick",eltype="xbrick",typekey=1198) 

        call prepare(lib_xbrick(1199),key=1199, & 
& nodecnc=[982,1012,983,961,3554,3584,3555,3533,18445, 18446,6396, 6395,18447, 18448,18242, 18241,18449 & 
& , 18450,6404, 6403,18451, 18452,18248, 18247], & 
& edgecnc=[6651,626,6652,6549,6653,630,6654,6552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1199),elname="xbrick",eltype="xbrick",typekey=1199) 

        call prepare(lib_xbrick(1200),key=1200, & 
& nodecnc=[2423,206,961,983,4995,2778,3533,3555,10382, 10381,17938, 17937,18448, 18447,18138, 18137,10388 & 
& , 10387,17944, 17943,18452, 18451,18142, 18141], & 
& edgecnc=[2619,6397,6652,6497,2622,6400,6654,6499], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1200),elname="xbrick",eltype="xbrick",typekey=1200) 

        call prepare(lib_xbrick(1201),key=1201, & 
& nodecnc=[2556,962,964,992,5128,3534,3536,3564,18078, 18077,17808, 17807,18453, 18454,18455, 18456,18086 & 
& , 18085,17812, 17811,18457, 18458,18459, 18460], & 
& edgecnc=[6467,6332,6655,6656,6471,6334,6657,6658], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1201),elname="xbrick",eltype="xbrick",typekey=1201) 

        call prepare(lib_xbrick(1202),key=1202, & 
& nodecnc=[1015,938,915,962,3587,3510,3487,3534,18461, 18462,17554, 17553,17806, 17805,18076, 18075,18463 & 
& , 18464,17560, 17559,17810, 17809,18084, 18083], & 
& edgecnc=[6659,6205,6331,6466,6660,6208,6333,6470], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1202),elname="xbrick",eltype="xbrick",typekey=1202) 

        call prepare(lib_xbrick(1203),key=1203, & 
& nodecnc=[1016,1005,963,1002,3588,3577,3535,3574,18465, 18466,18422, 18421,18467, 18468,18469, 18470 & 
& ,18471, 18472,18424, 18423,18473, 18474,18475, 18476], & 
& edgecnc=[6661,6639,6662,6663,6664,6640,6665,6666], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1203),elname="xbrick",eltype="xbrick",typekey=1203) 

        call prepare(lib_xbrick(1204),key=1204, & 
& nodecnc=[992,964,1076,1018,3564,3536,3648,3590,18454, 18453,18477, 18478,18479, 18480,18481, 18482,18458 & 
& , 18457,18483, 18484,18485, 18486,18487, 18488], & 
& edgecnc=[6655,6667,6668,6669,6657,6670,6671,6672], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1204),elname="xbrick",eltype="xbrick",typekey=1204) 

        call prepare(lib_xbrick(1205),key=1205, & 
& nodecnc=[1017,994,959,917,3589,3566,3531,3489,18489, 18490,17622, 17621,17844, 17843,17832, 17831,18491 & 
& , 18492,17630, 17629,17848, 17847,17838, 17837], & 
& edgecnc=[6673,6239,6350,6344,6674,6243,6352,6347], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1205),elname="xbrick",eltype="xbrick",typekey=1205) 

        call prepare(lib_xbrick(1206),key=1206, & 
& nodecnc=[976,1007,991,2476,3548,3579,3563,5048,18493, 18494,18495, 18496,18497, 18498,18122, 18121,18499 & 
& , 18500,18501, 18502,18503, 18504,18126, 18125], & 
& edgecnc=[6675,6676,6677,6489,6678,6679,6680,6491], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1206),elname="xbrick",eltype="xbrick",typekey=1206) 

        call prepare(lib_xbrick(1207),key=1207, & 
& nodecnc=[309,1023,996,1049,2881,3595,3568,3621,18505, 18506,18507, 18508,5538, 5537,18509, 18510,18511 & 
& , 18512,18513, 18514,5546, 5545,18515, 18516], & 
& edgecnc=[6681,6682,197,6683,6684,6685,201,6686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1207),elname="xbrick",eltype="xbrick",typekey=1207) 

        call prepare(lib_xbrick(1208),key=1208, & 
& nodecnc=[948,900,936,968,3520,3472,3508,3540,11236, 11235,17062, 17061,18254, 18253,18346, 18345,11244 & 
& , 11243,17070, 17069,18258, 18257,18352, 18351], & 
& edgecnc=[3046,5959,6555,6601,3050,5963,6557,6604], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1208),elname="xbrick",eltype="xbrick",typekey=1208) 

        call prepare(lib_xbrick(1209),key=1209, & 
& nodecnc=[2412,971,973,998,4984,3543,3545,3570,18517, 18518,18438, 18437,18519, 18520,5542, 5541,18521 & 
& , 18522,18442, 18441,18523, 18524,5550, 5549], & 
& edgecnc=[6687,6647,6688,199,6689,6649,6690,203], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1209),elname="xbrick",eltype="xbrick",typekey=1209) 

        call prepare(lib_xbrick(1210),key=1210, & 
& nodecnc=[2412,935,141,971,4984,3507,2713,3543,18196, 18195,18264, 18263,18440, 18439,18518, 18517,18200 & 
& , 18199,18268, 18267,18444, 18443,18522, 18521], & 
& edgecnc=[6526,6560,6648,6687,6528,6562,6650,6689], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1210),elname="xbrick",eltype="xbrick",typekey=1210) 

        call prepare(lib_xbrick(1211),key=1211, & 
& nodecnc=[1061,998,973,1022,3633,3570,3545,3594,18525, 18526,18520, 18519,8834, 8833,18527, 18528,18529 & 
& , 18530,18524, 18523,8842, 8841,18531, 18532], & 
& edgecnc=[6691,6688,1845,6692,6693,6690,1849,6694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1211),elname="xbrick",eltype="xbrick",typekey=1211) 

        call prepare(lib_xbrick(1212),key=1212, & 
& nodecnc=[902,970,975,1092,3474,3542,3547,3664,17634, 17633,18220, 18219,12958, 12957,17726, 17725,17640 & 
& , 17639,18224, 18223,12964, 12963,17730, 17729], & 
& edgecnc=[6245,6538,3907,6291,6248,6540,3910,6293], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1212),elname="xbrick",eltype="xbrick",typekey=1212) 

        call prepare(lib_xbrick(1213),key=1213, & 
& nodecnc=[1007,976,950,137,3579,3548,3522,2709,18494, 18493,17720, 17719,18226, 18225,18533, 18534,18500 & 
& , 18499,17724, 17723,18230, 18229,18535, 18536], & 
& edgecnc=[6675,6288,6541,6695,6678,6290,6543,6696], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1213),elname="xbrick",eltype="xbrick",typekey=1213) 

        call prepare(lib_xbrick(1214),key=1214, & 
& nodecnc=[1021,276,977,2283,3593,2848,3549,4855,17958, 17957,17694, 17693,18537, 18538,18164, 18163,17962 & 
& , 17961,17696, 17695,18539, 18540,18170, 18169], & 
& edgecnc=[6407,6275,6697,6510,6409,6276,6698,6513], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1214),elname="xbrick",eltype="xbrick",typekey=1214) 

        call prepare(lib_xbrick(1215),key=1215, & 
& nodecnc=[2225,1042,944,1014,4797,3614,3516,3586,12360, 12359,12340, 12339,18324, 18323,18541, 18542 & 
& ,12368, 12367,12348, 12347,18330, 18329,18543, 18544], & 
& edgecnc=[3608,3598,6590,6699,3612,3602,6593,6700], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1215),elname="xbrick",eltype="xbrick",typekey=1215) 

        call prepare(lib_xbrick(1216),key=1216, & 
& nodecnc=[2438,158,978,942,5010,2730,3550,3514,18274, 18273,18434, 18433,17898, 17897,8304, 8303,18280 & 
& , 18279,18436, 18435,17900, 17899,8312, 8311], & 
& edgecnc=[6565,6645,6377,1580,6568,6646,6378,1584], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1216),elname="xbrick",eltype="xbrick",typekey=1216) 

        call prepare(lib_xbrick(1217),key=1217, & 
& nodecnc=[993,986,954,953,3565,3558,3526,3525,11836, 11835,18052, 18051,18068, 18067,18545, 18546,11844 & 
& , 11843,18058, 18057,18072, 18071,18547, 18548], & 
& edgecnc=[3346,6454,6462,6701,3350,6457,6464,6702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1217),elname="xbrick",eltype="xbrick",typekey=1217) 

        call prepare(lib_xbrick(1218),key=1218, & 
& nodecnc=[1039,981,1047,2287,3611,3553,3619,4859,18549, 18550,18312, 18311,18551, 18552,18553, 18554 & 
& ,18555, 18556,18318, 18317,18557, 18558,18559, 18560], & 
& edgecnc=[6703,6584,6704,6705,6706,6587,6707,6708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1218),elname="xbrick",eltype="xbrick",typekey=1218) 

        call prepare(lib_xbrick(1219),key=1219, & 
& nodecnc=[1053,277,982,1051,3625,2849,3554,3623,18561, 18562,18563, 18564,18246, 18245,18166, 18165,18565 & 
& , 18566,18567, 18568,18252, 18251,18172, 18171], & 
& edgecnc=[6709,6710,6551,6511,6711,6712,6554,6514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1219),elname="xbrick",eltype="xbrick",typekey=1219) 

        call prepare(lib_xbrick(1220),key=1220, & 
& nodecnc=[2287,1036,1071,305,4859,3608,3643,2877,18569, 18570,18571, 18572,18573, 18574,18575, 18576 & 
& ,18577, 18578,18579, 18580,18581, 18582,18583, 18584], & 
& edgecnc=[6713,6714,6715,6716,6717,6718,6719,6720], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1220),elname="xbrick",eltype="xbrick",typekey=1220) 

        call prepare(lib_xbrick(1221),key=1221, & 
& nodecnc=[985,1076,964,939,3557,3648,3536,3511,18585, 18586,18478, 18477,17798, 17797,18278, 18277,18587 & 
& , 18588,18484, 18483,17804, 17803,18284, 18283], & 
& edgecnc=[6721,6667,6327,6567,6722,6670,6330,6570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1221),elname="xbrick",eltype="xbrick",typekey=1221) 

        call prepare(lib_xbrick(1222),key=1222, & 
& nodecnc=[1066,2482,995,1046,3638,5054,3567,3618,18589, 18590,18591, 18592,18593, 18594,8506, 8505,18595 & 
& , 18596,18597, 18598,18599, 18600,8514, 8513], & 
& edgecnc=[6723,6724,6725,1681,6726,6727,6728,1685], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1222),elname="xbrick",eltype="xbrick",typekey=1222) 

        call prepare(lib_xbrick(1223),key=1223, & 
& nodecnc=[2518,1077,1027,1073,5090,3649,3599,3645,18601, 18602,18603, 18604,18605, 18606,18607, 18608 & 
& ,18609, 18610,18611, 18612,18613, 18614,18615, 18616], & 
& edgecnc=[6729,6730,6731,6732,6733,6734,6735,6736], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1223),elname="xbrick",eltype="xbrick",typekey=1223) 

        call prepare(lib_xbrick(1224),key=1224, & 
& nodecnc=[989,2292,2327,2282,3561,4864,4899,4854,17740, 17739,18617, 18618,18619, 18620,18621, 18622 & 
& ,17744, 17743,18623, 18624,18625, 18626,18627, 18628], & 
& edgecnc=[6298,6737,6738,6739,6300,6740,6741,6742], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1224),elname="xbrick",eltype="xbrick",typekey=1224) 

        call prepare(lib_xbrick(1225),key=1225, & 
& nodecnc=[984,1047,943,990,3556,3619,3515,3562,18629, 18630,18310, 18309,17982, 17981,17966, 17965,18631 & 
& , 18632,18316, 18315,17988, 17987,17972, 17971], & 
& edgecnc=[6743,6583,6419,6411,6744,6586,6422,6414], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1225),elname="xbrick",eltype="xbrick",typekey=1225) 

        call prepare(lib_xbrick(1226),key=1226, & 
& nodecnc=[966,2476,1046,995,3538,5048,3618,3567,18124, 18123,18633, 18634,18594, 18593,8300, 8299,18128 & 
& , 18127,18635, 18636,18600, 18599,8308, 8307], & 
& edgecnc=[6490,6745,6725,1578,6492,6746,6728,1582], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1226),elname="xbrick",eltype="xbrick",typekey=1226) 

        call prepare(lib_xbrick(1227),key=1227, & 
& nodecnc=[2477,991,1007,1031,5049,3563,3579,3603,18637, 18638,18496, 18495,18639, 18640,18302, 18301 & 
& ,18641, 18642,18502, 18501,18643, 18644,18308, 18307], & 
& edgecnc=[6747,6676,6748,6579,6749,6679,6750,6582], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1227),elname="xbrick",eltype="xbrick",typekey=1227) 

        call prepare(lib_xbrick(1228),key=1228, & 
& nodecnc=[2562,1060,176,1044,5134,3632,2748,3616,8492, 8491,8476, 8475,18645, 18646,18647, 18648,8500 & 
& , 8499,8484, 8483,18649, 18650,18651, 18652], & 
& edgecnc=[1674,1666,6751,6752,1678,1670,6753,6754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1228),elname="xbrick",eltype="xbrick",typekey=1228) 

        call prepare(lib_xbrick(1229),key=1229, & 
& nodecnc=[2562,992,1018,1058,5134,3564,3590,3630,18653, 18654,18482, 18481,18655, 18656,8486, 8485,18657 & 
& , 18658,18488, 18487,18659, 18660,8494, 8493], & 
& edgecnc=[6755,6669,6756,1671,6757,6672,6758,1675], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1229),elname="xbrick",eltype="xbrick",typekey=1229) 

        call prepare(lib_xbrick(1230),key=1230, & 
& nodecnc=[1015,2561,1041,993,3587,5133,3613,3565,18074, 18073,18661, 18662,18663, 18664,18665, 18666 & 
& ,18082, 18081,18667, 18668,18669, 18670,18671, 18672], & 
& edgecnc=[6465,6759,6760,6761,6469,6762,6763,6764], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1230),elname="xbrick",eltype="xbrick",typekey=1230) 

        call prepare(lib_xbrick(1231),key=1231, & 
& nodecnc=[994,1017,1080,1000,3566,3589,3652,3572,18490, 18489,18673, 18674,18675, 18676,18014, 18013 & 
& ,18492, 18491,18677, 18678,18679, 18680,18018, 18017], & 
& edgecnc=[6673,6765,6766,6435,6674,6767,6768,6437], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1231),elname="xbrick",eltype="xbrick",typekey=1231) 

        call prepare(lib_xbrick(1232),key=1232, & 
& nodecnc=[985,2482,1028,1076,3557,5054,3600,3648,18681, 18682,18683, 18684,18685, 18686,18586, 18585 & 
& ,18687, 18688,18689, 18690,18691, 18692,18588, 18587], & 
& edgecnc=[6769,6770,6771,6721,6772,6773,6774,6722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1232),elname="xbrick",eltype="xbrick",typekey=1232) 

        call prepare(lib_xbrick(1233),key=1233, & 
& nodecnc=[1001,968,996,1023,3573,3540,3568,3595,18342, 18341,18256, 18255,18508, 18507,18693, 18694,18348 & 
& , 18347,18260, 18259,18514, 18513,18695, 18696], & 
& edgecnc=[6599,6556,6682,6775,6602,6558,6685,6776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1233),elname="xbrick",eltype="xbrick",typekey=1233) 

        call prepare(lib_xbrick(1234),key=1234, & 
& nodecnc=[1061,1139,1049,998,3633,3711,3621,3570,11298, 11297,18697, 18698,5544, 5543,18526, 18525,11306 & 
& , 11305,18699, 18700,5552, 5551,18530, 18529], & 
& edgecnc=[3077,6777,200,6691,3081,6778,204,6693], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1234),elname="xbrick",eltype="xbrick",typekey=1234) 

        call prepare(lib_xbrick(1235),key=1235, & 
& nodecnc=[1080,1059,1022,1000,3652,3631,3594,3572,6508, 6507,18701, 18702,8832, 8831,18676, 18675,6516 & 
& , 6515,18703, 18704,8840, 8839,18680, 18679], & 
& edgecnc=[682,6779,1844,6766,686,6780,1848,6768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1235),elname="xbrick",eltype="xbrick",typekey=1235) 

        call prepare(lib_xbrick(1236),key=1236, & 
& nodecnc=[1084,1001,1023,1062,3656,3573,3595,3634,11328, 11327,18694, 18693,18705, 18706,18707, 18708 & 
& ,11336, 11335,18696, 18695,18709, 18710,18711, 18712], & 
& edgecnc=[3092,6775,6781,6782,3096,6776,6783,6784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1236),elname="xbrick",eltype="xbrick",typekey=1236) 

        call prepare(lib_xbrick(1237),key=1237, & 
& nodecnc=[1077,1016,1002,1027,3649,3588,3574,3599,18713, 18714,18470, 18469,11832, 11831,18604, 18603 & 
& ,18715, 18716,18476, 18475,11840, 11839,18612, 18611], & 
& edgecnc=[6785,6663,3344,6730,6786,6666,3348,6734], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1237),elname="xbrick",eltype="xbrick",typekey=1237) 

        call prepare(lib_xbrick(1238),key=1238, & 
& nodecnc=[1045,1003,972,121,3617,3575,3544,2693,10366, 10365,18176, 18175,18717, 18718,18719, 18720,10372 & 
& , 10371,18180, 18179,18721, 18722,18723, 18724], & 
& edgecnc=[2611,6516,6787,6788,2614,6518,6789,6790], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1238),elname="xbrick",eltype="xbrick",typekey=1238) 

        call prepare(lib_xbrick(1239),key=1239, & 
& nodecnc=[1020,1075,2543,121,3592,3647,5115,2693,18725, 18726,18727, 18728,18729, 18730,18731, 18732 & 
& ,18733, 18734,18735, 18736,18737, 18738,18739, 18740], & 
& edgecnc=[6791,6792,6793,6794,6795,6796,6797,6798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1239),elname="xbrick",eltype="xbrick",typekey=1239) 

        call prepare(lib_xbrick(1240),key=1240, & 
& nodecnc=[1026,1172,2379,1004,3598,3744,4951,3576,18741, 18742,18743, 18744,18366, 18365,18745, 18746 & 
& ,18747, 18748,18749, 18750,18374, 18373,18751, 18752], & 
& edgecnc=[6799,6800,6611,6801,6802,6803,6615,6804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1240),elname="xbrick",eltype="xbrick",typekey=1240) 

        call prepare(lib_xbrick(1241),key=1241, & 
& nodecnc=[916,209,1026,1004,3488,2781,3598,3576,18092, 18091,8460, 8459,18746, 18745,18382, 18381,18096 & 
& , 18095,8468, 8467,18752, 18751,18384, 18383], & 
& edgecnc=[6474,1658,6801,6619,6476,1662,6804,6620], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1241),elname="xbrick",eltype="xbrick",typekey=1241) 

        call prepare(lib_xbrick(1242),key=1242, & 
& nodecnc=[1038,922,930,988,3610,3494,3502,3560,18753, 18754,17824, 17823,18214, 18213,6406, 6405,18755 & 
& , 18756,17828, 17827,18216, 18215,6414, 6413], & 
& edgecnc=[6805,6340,6535,631,6806,6342,6536,635], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1242),elname="xbrick",eltype="xbrick",typekey=1242) 

        call prepare(lib_xbrick(1243),key=1243, & 
& nodecnc=[1020,967,922,1038,3592,3539,3494,3610,18757, 18758,17428, 17427,18754, 18753,18759, 18760,18761 & 
& , 18762,17436, 17435,18756, 18755,18763, 18764], & 
& edgecnc=[6807,6142,6805,6808,6809,6146,6806,6810], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1243),elname="xbrick",eltype="xbrick",typekey=1243) 

        call prepare(lib_xbrick(1244),key=1244, & 
& nodecnc=[1095,1006,981,1039,3667,3578,3553,3611,11178, 11177,18426, 18425,18550, 18549,8284, 8283,11186 & 
& , 11185,18430, 18429,18556, 18555,8292, 8291], & 
& edgecnc=[3017,6641,6703,1570,3021,6643,6706,1574], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1244),elname="xbrick",eltype="xbrick",typekey=1244) 

        call prepare(lib_xbrick(1245),key=1245, & 
& nodecnc=[1006,1043,1007,137,3578,3615,3579,2709,11176, 11175,18765, 18766,18534, 18533,18428, 18427 & 
& ,11184, 11183,18767, 18768,18536, 18535,18432, 18431], & 
& edgecnc=[3016,6811,6695,6642,3020,6812,6696,6644], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1245),elname="xbrick",eltype="xbrick",typekey=1245) 

        call prepare(lib_xbrick(1246),key=1246, & 
& nodecnc=[1043,1124,1031,1007,3615,3696,3603,3579,11174, 11173,18769, 18770,18640, 18639,18766, 18765 & 
& ,11182, 11181,18771, 18772,18644, 18643,18768, 18767], & 
& edgecnc=[3015,6813,6748,6811,3019,6814,6750,6812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1246),elname="xbrick",eltype="xbrick",typekey=1246) 

        call prepare(lib_xbrick(1247),key=1247, & 
& nodecnc=[1034,1069,1094,1008,3606,3641,3666,3580,18773, 18774,18775, 18776,18777, 18778,18418, 18417 & 
& ,18779, 18780,18781, 18782,18783, 18784,18420, 18419], & 
& edgecnc=[6815,6816,6817,6637,6818,6819,6820,6638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1247),elname="xbrick",eltype="xbrick",typekey=1247) 

        call prepare(lib_xbrick(1248),key=1248, & 
& nodecnc=[1080,1065,1114,142,3652,3637,3686,2714,18785, 18786,18787, 18788,6490, 6489,6502, 6501,18789 & 
& , 18790,18791, 18792,6498, 6497,6510, 6509], & 
& edgecnc=[6821,6822,673,679,6823,6824,677,683], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1248),elname="xbrick",eltype="xbrick",typekey=1248) 

        call prepare(lib_xbrick(1249),key=1249, & 
& nodecnc=[987,1068,1132,1010,3559,3640,3704,3582,18290, 18289,18793, 18794,18795, 18796,18286, 18285 & 
& ,18294, 18293,18797, 18798,18799, 18800,18288, 18287], & 
& edgecnc=[6573,6825,6826,6571,6575,6827,6828,6572], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1249),elname="xbrick",eltype="xbrick",typekey=1249) 

        call prepare(lib_xbrick(1250),key=1250, & 
& nodecnc=[1036,1010,1132,1071,3608,3582,3704,3643,8590, 8589,18796, 18795,18801, 18802,18572, 18571,8598 & 
& , 8597,18800, 18799,18803, 18804,18580, 18579], & 
& edgecnc=[1723,6826,6829,6714,1727,6828,6830,6718], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1250),elname="xbrick",eltype="xbrick",typekey=1250) 

        call prepare(lib_xbrick(1251),key=1251, & 
& nodecnc=[1098,1127,1024,277,3670,3699,3596,2849,12386, 12385,18805, 18806,18807, 18808,18809, 18810 & 
& ,12392, 12391,18811, 18812,18813, 18814,18815, 18816], & 
& edgecnc=[3621,6831,6832,6833,3624,6834,6835,6836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1251),elname="xbrick",eltype="xbrick",typekey=1251) 

        call prepare(lib_xbrick(1252),key=1252, & 
& nodecnc=[1024,1012,982,277,3596,3584,3554,2849,12014, 12013,18446, 18445,18564, 18563,18808, 18807,12020 & 
& , 12019,18450, 18449,18568, 18567,18814, 18813], & 
& edgecnc=[3435,6651,6710,6832,3438,6653,6712,6835], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1252),elname="xbrick",eltype="xbrick",typekey=1252) 

        call prepare(lib_xbrick(1253),key=1253, & 
& nodecnc=[1099,1074,1013,1040,3671,3646,3585,3612,18817, 18818,6538, 6537,12356, 12355,18819, 18820,18821 & 
& , 18822,6546, 6545,12364, 12363,18823, 18824], & 
& edgecnc=[6837,697,3606,6838,6839,701,3610,6840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1253),elname="xbrick",eltype="xbrick",typekey=1253) 

        call prepare(lib_xbrick(1254),key=1254, & 
& nodecnc=[121,972,967,1020,2693,3544,3539,3592,18718, 18717,18132, 18131,18758, 18757,18732, 18731,18722 & 
& , 18721,18136, 18135,18762, 18761,18740, 18739], & 
& edgecnc=[6787,6494,6807,6794,6789,6496,6809,6798], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1254),elname="xbrick",eltype="xbrick",typekey=1254) 

        call prepare(lib_xbrick(1255),key=1255, & 
& nodecnc=[1482,1465,241,1645,4054,4037,2813,4217,18825, 18826,18827, 18828,18829, 18830,13304, 13303 & 
& ,18831, 18832,18833, 18834,18835, 18836,13312, 13311], & 
& edgecnc=[6841,6842,6843,4080,6844,6845,6846,4084], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1255),elname="xbrick",eltype="xbrick",typekey=1255) 

        call prepare(lib_xbrick(1256),key=1256, & 
& nodecnc=[1645,1014,979,240,4217,3586,3551,2812,18837, 18838,18322, 18321,18839, 18840,13306, 13305,18841 & 
& , 18842,18328, 18327,18843, 18844,13314, 13313], & 
& edgecnc=[6847,6589,6848,4081,6849,6592,6850,4085], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1256),elname="xbrick",eltype="xbrick",typekey=1256) 

        call prepare(lib_xbrick(1257),key=1257, & 
& nodecnc=[2569,2520,1033,2531,5141,5092,3605,5103,18845, 18846,6470, 6469,17994, 17993,9700, 9699,18847 & 
& , 18848,6478, 6477,18000, 17999,9708, 9707], & 
& edgecnc=[6851,663,6425,2278,6852,667,6428,2282], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1257),elname="xbrick",eltype="xbrick",typekey=1257) 

        call prepare(lib_xbrick(1258),key=1258, & 
& nodecnc=[1045,1055,2569,1019,3617,3627,5141,3591,18849, 18850,18851, 18852,9698, 9697,10368, 10367,18853 & 
& , 18854,18855, 18856,9706, 9705,10374, 10373], & 
& edgecnc=[6853,6854,2277,2612,6855,6856,2281,2615], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1258),elname="xbrick",eltype="xbrick",typekey=1258) 

        call prepare(lib_xbrick(1259),key=1259, & 
& nodecnc=[1041,1073,1027,993,3613,3645,3599,3565,18857, 18858,18606, 18605,11830, 11829,18664, 18663 & 
& ,18859, 18860,18614, 18613,11838, 11837,18670, 18669], & 
& edgecnc=[6857,6731,3343,6760,6858,6735,3347,6763], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1259),elname="xbrick",eltype="xbrick",typekey=1259) 

        call prepare(lib_xbrick(1260),key=1260, & 
& nodecnc=[1009,1065,1080,1017,3581,3637,3652,3589,6474, 6473,18786, 18785,18674, 18673,17830, 17829,6482 & 
& , 6481,18790, 18789,18678, 18677,17836, 17835], & 
& edgecnc=[665,6821,6765,6343,669,6823,6767,6346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1260),elname="xbrick",eltype="xbrick",typekey=1260) 

        call prepare(lib_xbrick(1261),key=1261, & 
& nodecnc=[1076,1028,1058,1018,3648,3600,3630,3590,18686, 18685,18861, 18862,18656, 18655,18480, 18479 & 
& ,18692, 18691,18863, 18864,18660, 18659,18486, 18485], & 
& edgecnc=[6771,6859,6756,6668,6774,6860,6758,6671], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1261),elname="xbrick",eltype="xbrick",typekey=1261) 

        call prepare(lib_xbrick(1262),key=1262, & 
& nodecnc=[1037,205,1054,1048,3609,2777,3626,3620,18865, 18866,18867, 18868,18869, 18870,18140, 18139 & 
& ,18871, 18872,18873, 18874,18875, 18876,18144, 18143], & 
& edgecnc=[6861,6862,6863,6498,6864,6865,6866,6500], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1262),elname="xbrick",eltype="xbrick",typekey=1262) 

        call prepare(lib_xbrick(1263),key=1263, & 
& nodecnc=[934,926,1021,1051,3506,3498,3593,3623,17922, 17921,17960, 17959,18162, 18161,18244, 18243,17926 & 
& , 17925,17964, 17963,18168, 18167,18250, 18249], & 
& edgecnc=[6389,6408,6509,6550,6391,6410,6512,6553], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1263),elname="xbrick",eltype="xbrick",typekey=1263) 

        call prepare(lib_xbrick(1264),key=1264, & 
& nodecnc=[1059,1081,1061,1022,3631,3653,3633,3594,18877, 18878,11300, 11299,18528, 18527,18702, 18701 & 
& ,18879, 18880,11308, 11307,18532, 18531,18704, 18703], & 
& edgecnc=[6867,3078,6692,6779,6868,3082,6694,6780], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1264),elname="xbrick",eltype="xbrick",typekey=1264) 

        call prepare(lib_xbrick(1265),key=1265, & 
& nodecnc=[205,1037,1011,1070,2777,3609,3583,3642,18866, 18865,6392, 6391,12012, 12011,18881, 18882,18872 & 
& , 18871,6400, 6399,12018, 12017,18883, 18884], & 
& edgecnc=[6861,624,3434,6869,6864,628,3437,6870], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1265),elname="xbrick",eltype="xbrick",typekey=1265) 

        call prepare(lib_xbrick(1266),key=1266, & 
& nodecnc=[341,1072,1025,1035,2913,3644,3597,3607,18885, 18886,18292, 18291,12956, 12955,18887, 18888 & 
& ,18889, 18890,18296, 18295,12962, 12961,18891, 18892], & 
& edgecnc=[6871,6574,3906,6872,6873,6576,3909,6874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1266),elname="xbrick",eltype="xbrick",typekey=1266) 

        call prepare(lib_xbrick(1267),key=1267, & 
& nodecnc=[273,2379,1172,1056,2845,4951,3744,3628,18893, 18894,18744, 18743,18895, 18896,18897, 18898 & 
& ,18899, 18900,18750, 18749,18901, 18902,18903, 18904], & 
& edgecnc=[6875,6800,6876,6877,6878,6803,6879,6880], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1267),elname="xbrick",eltype="xbrick",typekey=1267) 

        call prepare(lib_xbrick(1268),key=1268, & 
& nodecnc=[1105,1116,1056,1172,3677,3688,3628,3744,18905, 18906,18907, 18908,18896, 18895,18909, 18910 & 
& ,18911, 18912,18913, 18914,18902, 18901,18915, 18916], & 
& edgecnc=[6881,6882,6876,6883,6884,6885,6879,6886], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1268),elname="xbrick",eltype="xbrick",typekey=1268) 

        call prepare(lib_xbrick(1269),key=1269, & 
& nodecnc=[159,1028,2482,1066,2731,3600,5054,3638,18917, 18918,18684, 18683,18590, 18589,18919, 18920 & 
& ,18921, 18922,18690, 18689,18596, 18595,18923, 18924], & 
& edgecnc=[6887,6770,6723,6888,6889,6773,6726,6890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1269),elname="xbrick",eltype="xbrick",typekey=1269) 

        call prepare(lib_xbrick(1270),key=1270, & 
& nodecnc=[2444,1029,949,974,5016,3601,3521,3546,18370, 18369,12312, 12311,18202, 18201,18358, 18357,18378 & 
& , 18377,12320, 12319,18208, 18207,18362, 18361], & 
& edgecnc=[6613,3584,6529,6607,6617,3588,6532,6609], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1270),elname="xbrick",eltype="xbrick",typekey=1270) 

        call prepare(lib_xbrick(1271),key=1271, & 
& nodecnc=[2280,1030,1063,1089,4852,3602,3635,3661,8366, 8365,18925, 18926,18927, 18928,18929, 18930,8374 & 
& , 8373,18931, 18932,18933, 18934,18935, 18936], & 
& edgecnc=[1611,6891,6892,6893,1615,6894,6895,6896], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1271),elname="xbrick",eltype="xbrick",typekey=1271) 

        call prepare(lib_xbrick(1272),key=1272, & 
& nodecnc=[1107,1185,1144,2258,3679,3757,3716,4830,18937, 18938,18939, 18940,18941, 18942,18943, 18944 & 
& ,18945, 18946,18947, 18948,18949, 18950,18951, 18952], & 
& edgecnc=[6897,6898,6899,6900,6901,6902,6903,6904], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1272),elname="xbrick",eltype="xbrick",typekey=1272) 

        call prepare(lib_xbrick(1273),key=1273, & 
& nodecnc=[2477,1046,2476,991,5049,3618,5048,3563,8508, 8507,18634, 18633,18498, 18497,18638, 18637,8516 & 
& , 8515,18636, 18635,18504, 18503,18642, 18641], & 
& edgecnc=[1682,6745,6677,6747,1686,6746,6680,6749], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1273),elname="xbrick",eltype="xbrick",typekey=1273) 

        call prepare(lib_xbrick(1274),key=1274, & 
& nodecnc=[1121,1671,1124,1095,3693,4243,3696,3667,18953, 18954,18955, 18956,11180, 11179,8282, 8281,18957 & 
& , 18958,18959, 18960,11188, 11187,8290, 8289], & 
& edgecnc=[6905,6906,3018,1569,6907,6908,3022,1573], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1274),elname="xbrick",eltype="xbrick",typekey=1274) 

        call prepare(lib_xbrick(1275),key=1275, & 
& nodecnc=[2224,2214,240,979,4796,4786,2812,3551,18961, 18962,18963, 18964,18840, 18839,17776, 17775,18965 & 
& , 18966,18967, 18968,18844, 18843,17780, 17779], & 
& edgecnc=[6909,6910,6848,6316,6911,6912,6850,6318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1275),elname="xbrick",eltype="xbrick",typekey=1275) 

        call prepare(lib_xbrick(1276),key=1276, & 
& nodecnc=[897,1032,2214,2224,3469,3604,4786,4796,6666, 6665,18969, 18970,18962, 18961,12328, 12327,6674 & 
& , 6673,18971, 18972,18966, 18965,12336, 12335], & 
& edgecnc=[761,6913,6909,3592,765,6914,6911,3596], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1276),elname="xbrick",eltype="xbrick",typekey=1276) 

        call prepare(lib_xbrick(1277),key=1277, & 
& nodecnc=[1113,2569,1055,1103,3685,5141,3627,3675,18973, 18974,18852, 18851,18975, 18976,18977, 18978 & 
& ,18979, 18980,18856, 18855,18981, 18982,18983, 18984], & 
& edgecnc=[6915,6854,6916,6917,6918,6856,6919,6920], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1277),elname="xbrick",eltype="xbrick",typekey=1277) 

        call prepare(lib_xbrick(1278),key=1278, & 
& nodecnc=[1113,2521,2520,2569,3685,5093,5092,5141,18985, 18986,18987, 18988,18846, 18845,18974, 18973 & 
& ,18989, 18990,18991, 18992,18848, 18847,18980, 18979], & 
& edgecnc=[6921,6922,6851,6915,6923,6924,6852,6918], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1278),elname="xbrick",eltype="xbrick",typekey=1278) 

        call prepare(lib_xbrick(1279),key=1279, & 
& nodecnc=[1057,1083,1069,1034,3629,3655,3641,3606,18993, 18994,18995, 18996,18774, 18773,8744, 8743,18997 & 
& , 18998,18999, 19000,18780, 18779,8752, 8751], & 
& edgecnc=[6925,6926,6815,1800,6927,6928,6818,1804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1279),elname="xbrick",eltype="xbrick",typekey=1279) 

        call prepare(lib_xbrick(1280),key=1280, & 
& nodecnc=[1035,1008,1094,341,3607,3580,3666,2913,18412, 18411,18778, 18777,19001, 19002,18888, 18887 & 
& ,18416, 18415,18784, 18783,19003, 19004,18892, 18891], & 
& edgecnc=[6634,6817,6929,6872,6636,6820,6930,6874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1280),elname="xbrick",eltype="xbrick",typekey=1280) 

        call prepare(lib_xbrick(1281),key=1281, & 
& nodecnc=[2287,1047,984,1036,4859,3619,3556,3608,18552, 18551,18630, 18629,8592, 8591,18570, 18569,18558 & 
& , 18557,18632, 18631,8600, 8599,18578, 18577], & 
& edgecnc=[6704,6743,1724,6713,6707,6744,1728,6717], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1281),elname="xbrick",eltype="xbrick",typekey=1281) 

        call prepare(lib_xbrick(1282),key=1282, & 
& nodecnc=[1020,1038,1048,1054,3592,3610,3620,3626,18760, 18759,6412, 6411,18870, 18869,19005, 19006,18764 & 
& , 18763,6420, 6419,18876, 18875,19007, 19008], & 
& edgecnc=[6808,634,6863,6931,6810,638,6866,6932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1282),elname="xbrick",eltype="xbrick",typekey=1282) 

        call prepare(lib_xbrick(1283),key=1283, & 
& nodecnc=[1020,1054,1093,1075,3592,3626,3665,3647,19006, 19005,19009, 19010,19011, 19012,18726, 18725 & 
& ,19008, 19007,19013, 19014,19015, 19016,18734, 18733], & 
& edgecnc=[6931,6933,6934,6791,6932,6935,6936,6795], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1283),elname="xbrick",eltype="xbrick",typekey=1283) 

        call prepare(lib_xbrick(1284),key=1284, & 
& nodecnc=[1064,1039,2287,305,3636,3611,4859,2877,8286, 8285,18554, 18553,18576, 18575,8270, 8269,8294 & 
& , 8293,18560, 18559,18584, 18583,8278, 8277], & 
& edgecnc=[1571,6705,6716,1563,1575,6708,6720,1567], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1284),elname="xbrick",eltype="xbrick",typekey=1284) 

        call prepare(lib_xbrick(1285),key=1285, & 
& nodecnc=[241,1708,1040,2225,2813,4280,3612,4797,19017, 19018,19019, 19020,12354, 12353,19021, 19022 & 
& ,19023, 19024,19025, 19026,12362, 12361,19027, 19028], & 
& edgecnc=[6937,6938,3605,6939,6940,6941,3609,6942], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1285),elname="xbrick",eltype="xbrick",typekey=1285) 

        call prepare(lib_xbrick(1286),key=1286, & 
& nodecnc=[176,1073,1041,1044,2748,3645,3613,3616,19029, 19030,18858, 18857,19031, 19032,18646, 18645 & 
& ,19033, 19034,18860, 18859,19035, 19036,18650, 18649], & 
& edgecnc=[6943,6857,6944,6751,6945,6858,6946,6753], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1286),elname="xbrick",eltype="xbrick",typekey=1286) 

        call prepare(lib_xbrick(1287),key=1287, & 
& nodecnc=[1013,2283,977,1042,3585,4855,3549,3614,6536, 6535,18538, 18537,12342, 12341,12358, 12357,6544 & 
& , 6543,18540, 18539,12350, 12349,12366, 12365], & 
& edgecnc=[696,6697,3599,3607,700,6698,3603,3611], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1287),elname="xbrick",eltype="xbrick",typekey=1287) 

        call prepare(lib_xbrick(1288),key=1288, & 
& nodecnc=[1044,2556,992,2562,3616,5128,3564,5134,19037, 19038,18456, 18455,18654, 18653,18648, 18647 & 
& ,19039, 19040,18460, 18459,18658, 18657,18652, 18651], & 
& edgecnc=[6947,6656,6755,6752,6948,6658,6757,6754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1288),elname="xbrick",eltype="xbrick",typekey=1288) 

        call prepare(lib_xbrick(1289),key=1289, & 
& nodecnc=[1110,1055,1045,1091,3682,3627,3617,3663,19041, 19042,18850, 18849,19043, 19044,19045, 19046 & 
& ,19047, 19048,18854, 18853,19049, 19050,19051, 19052], & 
& edgecnc=[6949,6853,6950,6951,6952,6855,6953,6954], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1289),elname="xbrick",eltype="xbrick",typekey=1289) 

        call prepare(lib_xbrick(1290),key=1290, & 
& nodecnc=[1115,309,1049,1139,3687,2881,3621,3711,19053, 19054,18510, 18509,18698, 18697,19055, 19056 & 
& ,19057, 19058,18516, 18515,18700, 18699,19059, 19060], & 
& edgecnc=[6955,6683,6777,6956,6957,6686,6778,6958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1290),elname="xbrick",eltype="xbrick",typekey=1290) 

        call prepare(lib_xbrick(1291),key=1291, & 
& nodecnc=[1125,1052,999,2280,3697,3624,3571,4852,19061, 19062,17914, 17913,8368, 8367,19063, 19064,19065 & 
& , 19066,17918, 17917,8376, 8375,19067, 19068], & 
& edgecnc=[6959,6385,1612,6960,6961,6387,1616,6962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1291),elname="xbrick",eltype="xbrick",typekey=1291) 

        call prepare(lib_xbrick(1292),key=1292, & 
& nodecnc=[1098,277,1053,1074,3670,2849,3625,3646,18810, 18809,18562, 18561,6540, 6539,19069, 19070,18816 & 
& , 18815,18566, 18565,6548, 6547,19071, 19072], & 
& edgecnc=[6833,6709,698,6963,6836,6711,702,6964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1292),elname="xbrick",eltype="xbrick",typekey=1292) 

        call prepare(lib_xbrick(1293),key=1293, & 
& nodecnc=[1165,1103,1055,1110,3737,3675,3627,3682,19073, 19074,18976, 18975,19042, 19041,9774, 9773,19075 & 
& , 19076,18982, 18981,19048, 19047,9782, 9781], & 
& edgecnc=[6965,6916,6949,2315,6966,6919,6952,2319], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1293),elname="xbrick",eltype="xbrick",typekey=1293) 

        call prepare(lib_xbrick(1294),key=1294, & 
& nodecnc=[1117,273,1056,1090,3689,2845,3628,3662,19077, 19078,18898, 18897,19079, 19080,19081, 19082 & 
& ,19083, 19084,18904, 18903,19085, 19086,19087, 19088], & 
& edgecnc=[6967,6877,6968,6969,6970,6880,6971,6972], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1294),elname="xbrick",eltype="xbrick",typekey=1294) 

        call prepare(lib_xbrick(1295),key=1295, & 
& nodecnc=[1102,1058,1028,159,3674,3630,3600,2731,8488, 8487,18862, 18861,18918, 18917,19089, 19090,8496 & 
& , 8495,18864, 18863,18922, 18921,19091, 19092], & 
& edgecnc=[1672,6859,6887,6973,1676,6860,6889,6974], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1295),elname="xbrick",eltype="xbrick",typekey=1295) 

        call prepare(lib_xbrick(1296),key=1296, & 
& nodecnc=[1134,1081,1059,1119,3706,3653,3631,3691,19093, 19094,18878, 18877,6506, 6505,19095, 19096,19097 & 
& , 19098,18880, 18879,6514, 6513,19099, 19100], & 
& edgecnc=[6975,6867,681,6976,6977,6868,685,6978], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1296),elname="xbrick",eltype="xbrick",typekey=1296) 

        call prepare(lib_xbrick(1297),key=1297, & 
& nodecnc=[1139,1109,1214,1115,3711,3681,3786,3687,11296, 11295,19101, 19102,11316, 11315,19056, 19055 & 
& ,11304, 11303,19103, 19104,11324, 11323,19060, 19059], & 
& edgecnc=[3076,6979,3086,6956,3080,6980,3090,6958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1297),elname="xbrick",eltype="xbrick",typekey=1297) 

        call prepare(lib_xbrick(1298),key=1298, & 
& nodecnc=[1078,1084,2290,2294,3650,3656,4862,4866,11330, 11329,19105, 19106,19107, 19108,19109, 19110 & 
& ,11338, 11337,19111, 19112,19113, 19114,19115, 19116], & 
& edgecnc=[3093,6981,6982,6983,3097,6984,6985,6986], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1298),elname="xbrick",eltype="xbrick",typekey=1298) 

        call prepare(lib_xbrick(1299),key=1299, & 
& nodecnc=[1088,1062,1023,309,3660,3634,3595,2881,19117, 19118,18706, 18705,18506, 18505,19119, 19120 & 
& ,19121, 19122,18710, 18709,18512, 18511,19123, 19124], & 
& edgecnc=[6987,6781,6681,6988,6989,6783,6684,6990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1299),elname="xbrick",eltype="xbrick",typekey=1299) 

        call prepare(lib_xbrick(1300),key=1300, & 
& nodecnc=[1180,1107,2285,1117,3752,3679,4857,3689,19125, 19126,19127, 19128,19129, 19130,19131, 19132 & 
& ,19133, 19134,19135, 19136,19137, 19138,19139, 19140], & 
& edgecnc=[6991,6992,6993,6994,6995,6996,6997,6998], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1300),elname="xbrick",eltype="xbrick",typekey=1300) 

        call prepare(lib_xbrick(1301),key=1301, & 
& nodecnc=[2285,1063,1030,1104,4857,3635,3602,3676,19141, 19142,18926, 18925,12308, 12307,19143, 19144 & 
& ,19145, 19146,18932, 18931,12316, 12315,19147, 19148], & 
& edgecnc=[6999,6891,3582,7000,7001,6894,3586,7002], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1301),elname="xbrick",eltype="xbrick",typekey=1301) 

        call prepare(lib_xbrick(1302),key=1302, & 
& nodecnc=[1065,2520,2521,1114,3637,5092,5093,3686,6472, 6471,18988, 18987,19149, 19150,18788, 18787,6480 & 
& , 6479,18992, 18991,19151, 19152,18792, 18791], & 
& edgecnc=[664,6922,7003,6822,668,6924,7004,6824], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1302),elname="xbrick",eltype="xbrick",typekey=1302) 

        call prepare(lib_xbrick(1303),key=1303, & 
& nodecnc=[1130,159,1066,1112,3702,2731,3638,3684,19153, 19154,18920, 18919,19155, 19156,19157, 19158 & 
& ,19159, 19160,18924, 18923,19161, 19162,19163, 19164], & 
& edgecnc=[7005,6888,7006,7007,7008,6890,7009,7010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1303),elname="xbrick",eltype="xbrick",typekey=1303) 

        call prepare(lib_xbrick(1304),key=1304, & 
& nodecnc=[1204,1656,1067,1618,3776,4228,3639,4190,8380, 8379,19165, 19166,19167, 19168,19169, 19170,8386 & 
& , 8385,19171, 19172,19173, 19174,19175, 19176], & 
& edgecnc=[1618,7011,7012,7013,1621,7014,7015,7016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1304),elname="xbrick",eltype="xbrick",typekey=1304) 

        call prepare(lib_xbrick(1305),key=1305, & 
& nodecnc=[1052,1125,1618,1067,3624,3697,4190,3639,19062, 19061,19177, 19178,19168, 19167,17902, 17901 & 
& ,19066, 19065,19179, 19180,19174, 19173,17908, 17907], & 
& edgecnc=[6959,7017,7012,6379,6961,7018,7015,6382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1305),elname="xbrick",eltype="xbrick",typekey=1305) 

        call prepare(lib_xbrick(1306),key=1306, & 
& nodecnc=[308,1078,1083,1057,2880,3650,3655,3629,18354, 18353,19181, 19182,18994, 18993,18116, 18115 & 
& ,18356, 18355,19183, 19184,18998, 18997,18120, 18119], & 
& edgecnc=[6605,7019,6925,6486,6606,7020,6927,6488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1306),elname="xbrick",eltype="xbrick",typekey=1306) 

        call prepare(lib_xbrick(1307),key=1307, & 
& nodecnc=[1070,1616,1122,205,3642,4188,3694,2777,19185, 19186,12024, 12023,19187, 19188,18882, 18881 & 
& ,19189, 19190,12028, 12027,19191, 19192,18884, 18883], & 
& edgecnc=[7021,3440,7022,6869,7023,3442,7024,6870], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1307),elname="xbrick",eltype="xbrick",typekey=1307) 

        call prepare(lib_xbrick(1308),key=1308, & 
& nodecnc=[1616,1070,1024,1665,4188,3642,3596,4237,19186, 19185,12010, 12009,19193, 19194,10408, 10407 & 
& ,19190, 19189,12016, 12015,19195, 19196,10416, 10415], & 
& edgecnc=[7021,3433,7025,2632,7023,3436,7026,2636], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1308),elname="xbrick",eltype="xbrick",typekey=1308) 

        call prepare(lib_xbrick(1309),key=1309, & 
& nodecnc=[1228,1752,204,1237,3800,4324,2776,3809,9794, 9793,19197, 19198,19199, 19200,19201, 19202,9802 & 
& , 9801,19203, 19204,19205, 19206,19207, 19208], & 
& edgecnc=[2325,7027,7028,7029,2329,7030,7031,7032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1309),elname="xbrick",eltype="xbrick",typekey=1309) 

        call prepare(lib_xbrick(1310),key=1310, & 
& nodecnc=[1093,1086,2543,1075,3665,3658,5115,3647,10390, 10389,19209, 19210,18728, 18727,19012, 19011 & 
& ,10398, 10397,19211, 19212,18736, 18735,19016, 19015], & 
& edgecnc=[2623,7033,6792,6934,2627,7034,6796,6936], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1310),elname="xbrick",eltype="xbrick",typekey=1310) 

        call prepare(lib_xbrick(1311),key=1311, & 
& nodecnc=[1154,1156,1071,1132,3726,3728,3643,3704,8574, 8573,19213, 19214,18802, 18801,19215, 19216,8582 & 
& , 8581,19217, 19218,18804, 18803,19219, 19220], & 
& edgecnc=[1715,7035,6829,7036,1719,7037,6830,7038], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1311),elname="xbrick",eltype="xbrick",typekey=1311) 

        call prepare(lib_xbrick(1312),key=1312, & 
& nodecnc=[1072,341,1100,1111,3644,2913,3672,3683,18886, 18885,19221, 19222,19223, 19224,13776, 13775 & 
& ,18890, 18889,19225, 19226,19227, 19228,13782, 13781], & 
& edgecnc=[6871,7039,7040,4316,6873,7041,7042,4319], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1312),elname="xbrick",eltype="xbrick",typekey=1312) 

        call prepare(lib_xbrick(1313),key=1313, & 
& nodecnc=[176,1085,2518,1073,2748,3657,5090,3645,8474, 8473,19229, 19230,18608, 18607,19030, 19029,8482 & 
& , 8481,19231, 19232,18616, 18615,19034, 19033], & 
& edgecnc=[1665,7043,6732,6943,1669,7044,6736,6945], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1313),elname="xbrick",eltype="xbrick",typekey=1313) 

        call prepare(lib_xbrick(1314),key=1314, & 
& nodecnc=[1098,1074,1099,1129,3670,3646,3671,3701,19070, 19069,18818, 18817,19233, 19234,12382, 12381 & 
& ,19072, 19071,18822, 18821,19235, 19236,12388, 12387], & 
& edgecnc=[6963,6837,7045,3619,6964,6839,7046,3622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1314),elname="xbrick",eltype="xbrick",typekey=1314) 

        call prepare(lib_xbrick(1315),key=1315, & 
& nodecnc=[210,2442,1016,1077,2782,5014,3588,3649,19237, 19238,19239, 19240,18714, 18713,19241, 19242 & 
& ,19243, 19244,19245, 19246,18716, 18715,19247, 19248], & 
& edgecnc=[7047,7048,6785,7049,7050,7051,6786,7052], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1315),elname="xbrick",eltype="xbrick",typekey=1315) 

        call prepare(lib_xbrick(1316),key=1316, & 
& nodecnc=[1077,2518,1101,210,3649,5090,3673,2782,18602, 18601,19249, 19250,19251, 19252,19242, 19241 & 
& ,18610, 18609,19253, 19254,19255, 19256,19248, 19247], & 
& edgecnc=[6729,7053,7054,7049,6733,7055,7056,7052], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1316),elname="xbrick",eltype="xbrick",typekey=1316) 

        call prepare(lib_xbrick(1317),key=1317, & 
& nodecnc=[1299,1145,2296,2233,3871,3717,4868,4805,19257, 19258,19259, 19260,19261, 19262,11264, 11263 & 
& ,19263, 19264,19265, 19266,19267, 19268,11272, 11271], & 
& edgecnc=[7057,7058,7059,3060,7060,7061,7062,3064], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1317),elname="xbrick",eltype="xbrick",typekey=1317) 

        call prepare(lib_xbrick(1318),key=1318, & 
& nodecnc=[159,1130,1143,1102,2731,3702,3715,3674,19154, 19153,19269, 19270,19271, 19272,19090, 19089 & 
& ,19160, 19159,19273, 19274,19275, 19276,19092, 19091], & 
& edgecnc=[7005,7063,7064,6973,7008,7065,7066,6974], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1318),elname="xbrick",eltype="xbrick",typekey=1318) 

        call prepare(lib_xbrick(1319),key=1319, & 
& nodecnc=[1066,1082,1162,1112,3638,3654,3734,3684,8504, 8503,19277, 19278,11150, 11149,19156, 19155,8512 & 
& , 8511,19279, 19280,11158, 11157,19162, 19161], & 
& edgecnc=[1680,7067,3003,7006,1684,7068,3007,7009], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1319),elname="xbrick",eltype="xbrick",typekey=1319) 

        call prepare(lib_xbrick(1320),key=1320, & 
& nodecnc=[1153,1069,1083,342,3725,3641,3655,2914,19281, 19282,18996, 18995,19283, 19284,19285, 19286 & 
& ,19287, 19288,19000, 18999,19289, 19290,19291, 19292], & 
& edgecnc=[7069,6926,7070,7071,7072,6928,7073,7074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1320),elname="xbrick",eltype="xbrick",typekey=1320) 

        call prepare(lib_xbrick(1321),key=1321, & 
& nodecnc=[1118,2236,342,1083,3690,4808,2914,3655,19293, 19294,19295, 19296,19284, 19283,19297, 19298 & 
& ,19299, 19300,19301, 19302,19290, 19289,19303, 19304], & 
& edgecnc=[7075,7076,7070,7077,7078,7079,7073,7080], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1321),elname="xbrick",eltype="xbrick",typekey=1321) 

        call prepare(lib_xbrick(1322),key=1322, & 
& nodecnc=[1140,2233,2296,2290,3712,4805,4868,4862,19305, 19306,19262, 19261,19307, 19308,19309, 19310 & 
& ,19311, 19312,19268, 19267,19313, 19314,19315, 19316], & 
& edgecnc=[7081,7059,7082,7083,7084,7062,7085,7086], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1322),elname="xbrick",eltype="xbrick",typekey=1322) 

        call prepare(lib_xbrick(1323),key=1323, & 
& nodecnc=[1085,1133,1101,2518,3657,3705,3673,5090,12078, 12077,19317, 19318,19250, 19249,19230, 19229 & 
& ,12084, 12083,19319, 19320,19254, 19253,19232, 19231], & 
& edgecnc=[3467,7087,7053,7043,3470,7088,7055,7044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1323),elname="xbrick",eltype="xbrick",typekey=1323) 

        call prepare(lib_xbrick(1324),key=1324, & 
& nodecnc=[1086,1196,1091,2543,3658,3768,3663,5115,19321, 19322,19323, 19324,19325, 19326,19210, 19209 & 
& ,19327, 19328,19329, 19330,19331, 19332,19212, 19211], & 
& edgecnc=[7089,7090,7091,7033,7092,7093,7094,7034], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1324),elname="xbrick",eltype="xbrick",typekey=1324) 

        call prepare(lib_xbrick(1325),key=1325, & 
& nodecnc=[2503,1158,143,1135,5075,3730,2715,3707,19333, 19334,19335, 19336,19337, 19338,19339, 19340 & 
& ,19341, 19342,19343, 19344,19345, 19346,19347, 19348], & 
& edgecnc=[7095,7096,7097,7098,7099,7100,7101,7102], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1325),elname="xbrick",eltype="xbrick",typekey=1325) 

        call prepare(lib_xbrick(1326),key=1326, & 
& nodecnc=[309,1115,1138,1088,2881,3687,3710,3660,19054, 19053,11314, 11313,19349, 19350,19120, 19119 & 
& ,19058, 19057,11322, 11321,19351, 19352,19124, 19123], & 
& edgecnc=[6955,3085,7103,6988,6957,3089,7104,6990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1326),elname="xbrick",eltype="xbrick",typekey=1326) 

        call prepare(lib_xbrick(1327),key=1327, & 
& nodecnc=[1089,2251,1125,2280,3661,4823,3697,4852,19353, 19354,19355, 19356,19064, 19063,18930, 18929 & 
& ,19357, 19358,19359, 19360,19068, 19067,18936, 18935], & 
& edgecnc=[7105,7106,6960,6893,7107,7108,6962,6896], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1327),elname="xbrick",eltype="xbrick",typekey=1327) 

        call prepare(lib_xbrick(1328),key=1328, & 
& nodecnc=[273,1117,2285,1104,2845,3689,4857,3676,19078, 19077,19130, 19129,19144, 19143,19361, 19362 & 
& ,19084, 19083,19138, 19137,19148, 19147,19363, 19364], & 
& edgecnc=[6967,6993,7000,7109,6970,6997,7002,7110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1328),elname="xbrick",eltype="xbrick",typekey=1328) 

        call prepare(lib_xbrick(1329),key=1329, & 
& nodecnc=[1116,1147,272,1141,3688,3719,2844,3713,19365, 19366,19367, 19368,19369, 19370,19371, 19372 & 
& ,19373, 19374,19375, 19376,19377, 19378,19379, 19380], & 
& edgecnc=[7111,7112,7113,7114,7115,7116,7117,7118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1329),elname="xbrick",eltype="xbrick",typekey=1329) 

        call prepare(lib_xbrick(1330),key=1330, & 
& nodecnc=[1110,1091,1196,120,3682,3663,3768,2692,19046, 19045,19324, 19323,19381, 19382,9776, 9775,19052 & 
& , 19051,19330, 19329,19383, 19384,9784, 9783], & 
& edgecnc=[6951,7090,7119,2316,6954,7093,7120,2320], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1330),elname="xbrick",eltype="xbrick",typekey=1330) 

        call prepare(lib_xbrick(1331),key=1331, & 
& nodecnc=[1122,1093,1054,205,3694,3665,3626,2777,10392, 10391,19010, 19009,18868, 18867,19188, 19187 & 
& ,10400, 10399,19014, 19013,18874, 18873,19192, 19191], & 
& edgecnc=[2624,6933,6862,7022,2628,6935,6865,7024], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1331),elname="xbrick",eltype="xbrick",typekey=1331) 

        call prepare(lib_xbrick(1332),key=1332, & 
& nodecnc=[1100,341,1094,1120,3672,2913,3666,3692,19222, 19221,19002, 19001,19385, 19386,8730, 8729,19226 & 
& , 19225,19004, 19003,19387, 19388,8738, 8737], & 
& edgecnc=[7039,6929,7121,1793,7041,6930,7122,1797], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1332),elname="xbrick",eltype="xbrick",typekey=1332) 

        call prepare(lib_xbrick(1333),key=1333, & 
& nodecnc=[1120,1094,1069,1153,3692,3666,3641,3725,19386, 19385,18776, 18775,19282, 19281,11250, 11249 & 
& ,19388, 19387,18782, 18781,19288, 19287,11256, 11255], & 
& edgecnc=[7121,6816,7069,3053,7122,6819,7072,3056], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1333),elname="xbrick",eltype="xbrick",typekey=1333) 

        call prepare(lib_xbrick(1334),key=1334, & 
& nodecnc=[1096,1166,1612,1151,3668,3738,4184,3723,8266, 8265,19389, 19390,19391, 19392,19393, 19394,8274 & 
& , 8273,19395, 19396,19397, 19398,19399, 19400], & 
& edgecnc=[1561,7123,7124,7125,1565,7126,7127,7128], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1334),elname="xbrick",eltype="xbrick",typekey=1334) 

        call prepare(lib_xbrick(1335),key=1335, & 
& nodecnc=[305,1071,1156,1166,2877,3643,3728,3738,18574, 18573,19214, 19213,19401, 19402,8272, 8271,18582 & 
& , 18581,19218, 19217,19403, 19404,8280, 8279], & 
& edgecnc=[6715,7035,7129,1564,6719,7037,7130,1568], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1335),elname="xbrick",eltype="xbrick",typekey=1335) 

        call prepare(lib_xbrick(1336),key=1336, & 
& nodecnc=[1611,1099,1040,1708,4183,3671,3612,4280,19405, 19406,18820, 18819,19020, 19019,6524, 6523,19407 & 
& , 19408,18824, 18823,19026, 19025,6532, 6531], & 
& edgecnc=[7131,6838,6938,690,7132,6840,6941,694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1336),elname="xbrick",eltype="xbrick",typekey=1336) 

        call prepare(lib_xbrick(1337),key=1337, & 
& nodecnc=[1170,461,71,1646,3742,3033,2643,4218,19409, 19410,19411, 19412,19413, 19414,8726, 8725,19415 & 
& , 19416,19417, 19418,19419, 19420,8734, 8733], & 
& edgecnc=[7133,7134,7135,1791,7136,7137,7138,1795], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1337),elname="xbrick",eltype="xbrick",typekey=1337) 

        call prepare(lib_xbrick(1338),key=1338, & 
& nodecnc=[1131,210,1101,1123,3703,2782,3673,3695,19421, 19422,19252, 19251,19423, 19424,11060, 11059 & 
& ,19425, 19426,19256, 19255,19427, 19428,11068, 11067], & 
& edgecnc=[7139,7054,7140,2958,7141,7056,7142,2962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1338),elname="xbrick",eltype="xbrick",typekey=1338) 

        call prepare(lib_xbrick(1339),key=1339, & 
& nodecnc=[2390,211,1289,1288,4962,2783,3861,3860,19429, 19430,19431, 19432,19433, 19434,19435, 19436 & 
& ,19437, 19438,19439, 19440,19441, 19442,19443, 19444], & 
& edgecnc=[7143,7144,7145,7146,7147,7148,7149,7150], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1339),elname="xbrick",eltype="xbrick",typekey=1339) 

        call prepare(lib_xbrick(1340),key=1340, & 
& nodecnc=[1178,97,1103,1165,3750,2669,3675,3737,19445, 19446,19447, 19448,19074, 19073,19449, 19450,19451 & 
& , 19452,19453, 19454,19076, 19075,19455, 19456], & 
& edgecnc=[7151,7152,6965,7153,7154,7155,6966,7156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1340),elname="xbrick",eltype="xbrick",typekey=1340) 

        call prepare(lib_xbrick(1341),key=1341, & 
& nodecnc=[2379,273,1104,1029,4951,2845,3676,3601,18894, 18893,19362, 19361,12306, 12305,18368, 18367 & 
& ,18900, 18899,19364, 19363,12314, 12313,18376, 18375], & 
& edgecnc=[6875,7109,3581,6612,6878,7110,3585,6616], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1341),elname="xbrick",eltype="xbrick",typekey=1341) 

        call prepare(lib_xbrick(1342),key=1342, & 
& nodecnc=[1105,2395,1147,1116,3677,4967,3719,3688,12046, 12045,19457, 19458,19366, 19365,18906, 18905 & 
& ,12054, 12053,19459, 19460,19374, 19373,18912, 18911], & 
& edgecnc=[3451,7157,7111,6881,3455,7158,7115,6884], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1342),elname="xbrick",eltype="xbrick",typekey=1342) 

        call prepare(lib_xbrick(1343),key=1343, & 
& nodecnc=[1288,1277,177,1269,3860,3849,2749,3841,19461, 19462,19463, 19464,11076, 11075,19465, 19466 & 
& ,19467, 19468,19469, 19470,11082, 11081,19471, 19472], & 
& edgecnc=[7159,7160,2966,7161,7162,7163,2969,7164], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1343),elname="xbrick",eltype="xbrick",typekey=1343) 

        call prepare(lib_xbrick(1344),key=1344, & 
& nodecnc=[1106,1079,1128,1137,3678,3651,3700,3709,12080, 12079,11484, 11483,19473, 19474,19475, 19476 & 
& ,12086, 12085,11488, 11487,19477, 19478,19479, 19480], & 
& edgecnc=[3468,3170,7165,7166,3471,3172,7167,7168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1344),elname="xbrick",eltype="xbrick",typekey=1344) 

        call prepare(lib_xbrick(1345),key=1345, & 
& nodecnc=[2251,1089,1063,2258,4823,3661,3635,4830,19354, 19353,18928, 18927,19481, 19482,19483, 19484 & 
& ,19358, 19357,18934, 18933,19485, 19486,19487, 19488], & 
& edgecnc=[7105,6892,7169,7170,7107,6895,7171,7172], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1345),elname="xbrick",eltype="xbrick",typekey=1345) 

        call prepare(lib_xbrick(1346),key=1346, & 
& nodecnc=[1090,1164,1180,1117,3662,3736,3752,3689,8438, 8437,19489, 19490,19132, 19131,19082, 19081,8446 & 
& , 8445,19491, 19492,19140, 19139,19088, 19087], & 
& edgecnc=[1647,7173,6994,6969,1651,7174,6998,6972], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1346),elname="xbrick",eltype="xbrick",typekey=1346) 

        call prepare(lib_xbrick(1347),key=1347, & 
& nodecnc=[1081,1134,310,1109,3653,3706,2882,3681,19094, 19093,8822, 8821,19493, 19494,11294, 11293,19098 & 
& , 19097,8828, 8827,19495, 19496,11302, 11301], & 
& edgecnc=[6975,1839,7175,3075,6977,1842,7176,3079], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1347),elname="xbrick",eltype="xbrick",typekey=1347) 

        call prepare(lib_xbrick(1348),key=1348, & 
& nodecnc=[1165,1146,1195,1178,3737,3718,3767,3750,9780, 9779,19497, 19498,19499, 19500,19450, 19449,9788 & 
& , 9787,19501, 19502,19503, 19504,19456, 19455], & 
& edgecnc=[2318,7177,7178,7153,2322,7179,7180,7156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1348),elname="xbrick",eltype="xbrick",typekey=1348) 

        call prepare(lib_xbrick(1349),key=1349, & 
& nodecnc=[1614,1087,2503,1135,4186,3659,5075,3707,6486, 6485,19505, 19506,19340, 19339,19507, 19508,6494 & 
& , 6493,19509, 19510,19348, 19347,19511, 19512], & 
& edgecnc=[671,7181,7098,7182,675,7183,7102,7184], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1349),elname="xbrick",eltype="xbrick",typekey=1349) 

        call prepare(lib_xbrick(1350),key=1350, & 
& nodecnc=[97,1136,1113,1103,2669,3708,3685,3675,19513, 19514,19515, 19516,18978, 18977,19448, 19447,19517 & 
& , 19518,19519, 19520,18984, 18983,19454, 19453], & 
& edgecnc=[7185,7186,6917,7152,7187,7188,6920,7155], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1350),elname="xbrick",eltype="xbrick",typekey=1350) 

        call prepare(lib_xbrick(1351),key=1351, & 
& nodecnc=[1160,1119,142,1142,3732,3691,2714,3714,19521, 19522,6504, 6503,19523, 19524,19525, 19526,19527 & 
& , 19528,6512, 6511,19529, 19530,19531, 19532], & 
& edgecnc=[7189,680,7190,7191,7192,684,7193,7194], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1351),elname="xbrick",eltype="xbrick",typekey=1351) 

        call prepare(lib_xbrick(1352),key=1352, & 
& nodecnc=[1183,1142,142,1614,3755,3714,2714,4186,19533, 19534,19524, 19523,6488, 6487,19535, 19536,19537 & 
& , 19538,19530, 19529,6496, 6495,19539, 19540], & 
& edgecnc=[7195,7190,672,7196,7197,7193,676,7198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1352),elname="xbrick",eltype="xbrick",typekey=1352) 

        call prepare(lib_xbrick(1353),key=1353, & 
& nodecnc=[1083,1078,2294,1118,3655,3650,4866,3690,19182, 19181,19110, 19109,19541, 19542,19298, 19297 & 
& ,19184, 19183,19116, 19115,19543, 19544,19304, 19303], & 
& edgecnc=[7019,6983,7199,7077,7020,6986,7200,7080], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1353),elname="xbrick",eltype="xbrick",typekey=1353) 

        call prepare(lib_xbrick(1354),key=1354, & 
& nodecnc=[1062,1088,343,1140,3634,3660,2915,3712,19118, 19117,19545, 19546,19547, 19548,19549, 19550 & 
& ,19122, 19121,19551, 19552,19553, 19554,19555, 19556], & 
& edgecnc=[6987,7201,7202,7203,6989,7204,7205,7206], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1354),elname="xbrick",eltype="xbrick",typekey=1354) 

        call prepare(lib_xbrick(1355),key=1355, & 
& nodecnc=[1141,1090,1056,1116,3713,3662,3628,3688,8440, 8439,19080, 19079,18908, 18907,19372, 19371,8448 & 
& , 8447,19086, 19085,18914, 18913,19380, 19379], & 
& edgecnc=[1648,6968,6882,7114,1652,6971,6885,7118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1355),elname="xbrick",eltype="xbrick",typekey=1355) 

        call prepare(lib_xbrick(1356),key=1356, & 
& nodecnc=[1157,1134,1119,1160,3729,3706,3691,3732,8818, 8817,19096, 19095,19522, 19521,19557, 19558,8824 & 
& , 8823,19100, 19099,19528, 19527,19559, 19560], & 
& edgecnc=[1837,6976,7189,7207,1840,6978,7192,7208], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1356),elname="xbrick",eltype="xbrick",typekey=1356) 

        call prepare(lib_xbrick(1357),key=1357, & 
& nodecnc=[1142,1183,1175,1160,3714,3755,3747,3732,19534, 19533,9742, 9741,19561, 19562,19526, 19525,19538 & 
& , 19537,9750, 9749,19563, 19564,19532, 19531], & 
& edgecnc=[7195,2299,7209,7191,7197,2303,7210,7194], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1357),elname="xbrick",eltype="xbrick",typekey=1357) 

        call prepare(lib_xbrick(1358),key=1358, & 
& nodecnc=[461,1170,1159,462,3033,3742,3731,3034,19410, 19409,11254, 11253,19565, 19566,19567, 19568,19416 & 
& , 19415,11260, 11259,19569, 19570,19571, 19572], & 
& edgecnc=[7133,3055,7211,7212,7136,3058,7213,7214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1358),elname="xbrick",eltype="xbrick",typekey=1358) 

        call prepare(lib_xbrick(1359),key=1359, & 
& nodecnc=[1150,1121,1126,2413,3722,3693,3698,4985,19573, 19574,12712, 12711,19575, 19576,11190, 11189 & 
& ,19577, 19578,12716, 12715,19579, 19580,11196, 11195], & 
& edgecnc=[7215,3784,7216,3023,7217,3786,7218,3026], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1359),elname="xbrick",eltype="xbrick",typekey=1359) 

        call prepare(lib_xbrick(1360),key=1360, & 
& nodecnc=[1620,1771,1867,278,4192,4343,4439,2850,19581, 19582,19583, 19584,19585, 19586,12518, 12517 & 
& ,19587, 19588,19589, 19590,19591, 19592,12524, 12523], & 
& edgecnc=[7219,7220,7221,3687,7222,7223,7224,3690], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1360),elname="xbrick",eltype="xbrick",typekey=1360) 

        call prepare(lib_xbrick(1361),key=1361, & 
& nodecnc=[1106,1277,2417,1133,3678,3849,4989,3705,19593, 19594,19595, 19596,19597, 19598,12082, 12081 & 
& ,19599, 19600,19601, 19602,19603, 19604,12088, 12087], & 
& edgecnc=[7225,7226,7227,3469,7228,7229,7230,3472], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1361),elname="xbrick",eltype="xbrick",typekey=1361) 

        call prepare(lib_xbrick(1362),key=1362, & 
& nodecnc=[1671,136,1031,1124,4243,2708,3603,3696,19605, 19606,18298, 18297,18770, 18769,18956, 18955 & 
& ,19607, 19608,18304, 18303,18772, 18771,18960, 18959], & 
& edgecnc=[7231,6577,6813,6906,7232,6580,6814,6908], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1362),elname="xbrick",eltype="xbrick",typekey=1362) 

        call prepare(lib_xbrick(1363),key=1363, & 
& nodecnc=[1621,2447,136,1671,4193,5019,2708,4243,19609, 19610,19611, 19612,19606, 19605,19613, 19614 & 
& ,19615, 19616,19617, 19618,19608, 19607,19619, 19620], & 
& edgecnc=[7233,7234,7231,7235,7236,7237,7232,7238], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1363),elname="xbrick",eltype="xbrick",typekey=1363) 

        call prepare(lib_xbrick(1364),key=1364, & 
& nodecnc=[2258,1144,2235,2251,4830,3716,4807,4823,18942, 18941,19621, 19622,19623, 19624,19484, 19483 & 
& ,18950, 18949,19625, 19626,19627, 19628,19488, 19487], & 
& edgecnc=[6899,7239,7240,7170,6903,7241,7242,7172], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1364),elname="xbrick",eltype="xbrick",typekey=1364) 

        call prepare(lib_xbrick(1365),key=1365, & 
& nodecnc=[2414,2413,1126,1148,4986,4985,3698,3720,11192, 11191,19576, 19575,19629, 19630,19631, 19632 & 
& ,11198, 11197,19580, 19579,19633, 19634,19635, 19636], & 
& edgecnc=[3024,7216,7243,7244,3027,7218,7245,7246], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1365),elname="xbrick",eltype="xbrick",typekey=1365) 

        call prepare(lib_xbrick(1366),key=1366, & 
& nodecnc=[1096,1151,1148,1126,3668,3723,3720,3698,19394, 19393,19637, 19638,19630, 19629,12710, 12709 & 
& ,19400, 19399,19639, 19640,19634, 19633,12714, 12713], & 
& edgecnc=[7125,7247,7243,3783,7128,7248,7245,3785], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1366),elname="xbrick",eltype="xbrick",typekey=1366) 

        call prepare(lib_xbrick(1367),key=1367, & 
& nodecnc=[1024,1127,1815,1665,3596,3699,4387,4237,18806, 18805,12372, 12371,19641, 19642,19194, 19193 & 
& ,18812, 18811,12378, 12377,19643, 19644,19196, 19195], & 
& edgecnc=[6831,3614,7249,7025,6834,3617,7250,7026], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1367),elname="xbrick",eltype="xbrick",typekey=1367) 

        call prepare(lib_xbrick(1368),key=1368, & 
& nodecnc=[243,1169,1129,242,2815,3741,3701,2814,10428, 10427,12384, 12383,19645, 19646,19647, 19648,10436 & 
& , 10435,12390, 12389,19649, 19650,19651, 19652], & 
& edgecnc=[2642,3620,7251,7252,2646,3623,7253,7254], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1368),elname="xbrick",eltype="xbrick",typekey=1368) 

        call prepare(lib_xbrick(1369),key=1369, & 
& nodecnc=[1243,177,1137,1250,3815,2749,3709,3822,11078, 11077,19653, 19654,19655, 19656,11862, 11861 & 
& ,11084, 11083,19657, 19658,19659, 19660,11868, 11867], & 
& edgecnc=[2967,7255,7256,3359,2970,7257,7258,3362], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1369),elname="xbrick",eltype="xbrick",typekey=1369) 

        call prepare(lib_xbrick(1370),key=1370, & 
& nodecnc=[1611,242,1129,1099,4183,2814,3701,3671,19661, 19662,19646, 19645,19234, 19233,19406, 19405 & 
& ,19663, 19664,19650, 19649,19236, 19235,19408, 19407], & 
& edgecnc=[7259,7251,7045,7131,7260,7253,7046,7132], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1370),elname="xbrick",eltype="xbrick",typekey=1370) 

        call prepare(lib_xbrick(1371),key=1371, & 
& nodecnc=[1112,2540,1161,1130,3684,5112,3733,3702,11148, 11147,19665, 19666,19667, 19668,19158, 19157 & 
& ,11156, 11155,19669, 19670,19671, 19672,19164, 19163], & 
& edgecnc=[3002,7261,7262,7007,3006,7263,7264,7010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1371),elname="xbrick",eltype="xbrick",typekey=1371) 

        call prepare(lib_xbrick(1372),key=1372, & 
& nodecnc=[1319,1333,1634,1332,3891,3905,4206,3904,19673, 19674,19675, 19676,19677, 19678,19679, 19680 & 
& ,19681, 19682,19683, 19684,19685, 19686,19687, 19688], & 
& edgecnc=[7265,7266,7267,7268,7269,7270,7271,7272], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1372),elname="xbrick",eltype="xbrick",typekey=1372) 

        call prepare(lib_xbrick(1373),key=1373, & 
& nodecnc=[210,1131,2395,2442,2782,3703,4967,5014,19422, 19421,19689, 19690,12052, 12051,19238, 19237 & 
& ,19426, 19425,19691, 19692,12060, 12059,19244, 19243], & 
& edgecnc=[7139,7273,3454,7047,7141,7274,3458,7050], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1373),elname="xbrick",eltype="xbrick",typekey=1373) 

        call prepare(lib_xbrick(1374),key=1374, & 
& nodecnc=[1154,1132,1068,340,3726,3704,3640,2912,19216, 19215,18794, 18793,13778, 13777,13768, 13767 & 
& ,19220, 19219,18798, 18797,13784, 13783,13772, 13771], & 
& edgecnc=[7036,6825,4317,4312,7038,6827,4320,4314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1374),elname="xbrick",eltype="xbrick",typekey=1374) 

        call prepare(lib_xbrick(1375),key=1375, & 
& nodecnc=[2417,1123,1101,1133,4989,3695,3673,3705,19693, 19694,19424, 19423,19318, 19317,19598, 19597 & 
& ,19695, 19696,19428, 19427,19320, 19319,19604, 19603], & 
& edgecnc=[7275,7140,7087,7227,7276,7142,7088,7230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1375),elname="xbrick",eltype="xbrick",typekey=1375) 

        call prepare(lib_xbrick(1376),key=1376, & 
& nodecnc=[1190,1158,2503,1136,3762,3730,5075,3708,9762, 9761,19334, 19333,19697, 19698,19699, 19700,9770 & 
& , 9769,19342, 19341,19701, 19702,19703, 19704], & 
& edgecnc=[2309,7095,7277,7278,2313,7099,7279,7280], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1376),elname="xbrick",eltype="xbrick",typekey=1376) 

        call prepare(lib_xbrick(1377),key=1377, & 
& nodecnc=[2503,1087,1113,1136,5075,3659,3685,3708,19506, 19505,19705, 19706,19516, 19515,19698, 19697 & 
& ,19510, 19509,19707, 19708,19520, 19519,19702, 19701], & 
& edgecnc=[7181,7281,7186,7277,7183,7282,7188,7279], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1377),elname="xbrick",eltype="xbrick",typekey=1377) 

        call prepare(lib_xbrick(1378),key=1378, & 
& nodecnc=[1277,1106,1137,177,3849,3678,3709,2749,19594, 19593,19476, 19475,19654, 19653,19464, 19463 & 
& ,19600, 19599,19480, 19479,19658, 19657,19470, 19469], & 
& edgecnc=[7225,7166,7255,7160,7228,7168,7257,7163], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1378),elname="xbrick",eltype="xbrick",typekey=1378) 

        call prepare(lib_xbrick(1379),key=1379, & 
& nodecnc=[1088,1138,1179,343,3660,3710,3751,2915,19350, 19349,19709, 19710,19711, 19712,19546, 19545 & 
& ,19352, 19351,19713, 19714,19715, 19716,19552, 19551], & 
& edgecnc=[7103,7283,7284,7201,7104,7285,7286,7204], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1379),elname="xbrick",eltype="xbrick",typekey=1379) 

        call prepare(lib_xbrick(1380),key=1380, & 
& nodecnc=[1233,1179,1138,1163,3805,3751,3710,3735,19717, 19718,19710, 19709,11312, 11311,19719, 19720 & 
& ,19721, 19722,19714, 19713,11320, 11319,19723, 19724], & 
& edgecnc=[7287,7283,3084,7288,7289,7285,3088,7290], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1380),elname="xbrick",eltype="xbrick",typekey=1380) 

        call prepare(lib_xbrick(1381),key=1381, & 
& nodecnc=[2362,1319,272,1147,4934,3891,2844,3719,19725, 19726,19727, 19728,19368, 19367,19729, 19730 & 
& ,19731, 19732,19733, 19734,19376, 19375,19735, 19736], & 
& edgecnc=[7291,7292,7112,7293,7294,7295,7116,7296], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1381),elname="xbrick",eltype="xbrick",typekey=1381) 

        call prepare(lib_xbrick(1382),key=1382, & 
& nodecnc=[1614,1135,143,1183,4186,3707,2715,3755,19508, 19507,19338, 19337,9744, 9743,19536, 19535,19512 & 
& , 19511,19346, 19345,9752, 9751,19540, 19539], & 
& edgecnc=[7182,7097,2300,7196,7184,7101,2304,7198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1382),elname="xbrick",eltype="xbrick",typekey=1382) 

        call prepare(lib_xbrick(1383),key=1383, & 
& nodecnc=[1130,1161,1230,1143,3702,3733,3802,3715,19668, 19667,11092, 11091,19737, 19738,19270, 19269 & 
& ,19672, 19671,11100, 11099,19739, 19740,19274, 19273], & 
& edgecnc=[7262,2974,7297,7063,7264,2978,7298,7065], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1383),elname="xbrick",eltype="xbrick",typekey=1383) 

        call prepare(lib_xbrick(1384),key=1384, & 
& nodecnc=[239,2235,1144,1171,2811,4807,3716,3743,19741, 19742,19622, 19621,19743, 19744,8406, 8405,19745 & 
& , 19746,19626, 19625,19747, 19748,8414, 8413], & 
& edgecnc=[7299,7239,7300,1631,7301,7241,7302,1635], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1384),elname="xbrick",eltype="xbrick",typekey=1384) 

        call prepare(lib_xbrick(1385),key=1385, & 
& nodecnc=[1309,2236,1145,1299,3881,4808,3717,3871,19749, 19750,19751, 19752,19258, 19257,13788, 13787 & 
& ,19753, 19754,19755, 19756,19264, 19263,13794, 13793], & 
& edgecnc=[7303,7304,7057,4322,7305,7306,7060,4325], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1385),elname="xbrick",eltype="xbrick",typekey=1385) 

        call prepare(lib_xbrick(1386),key=1386, & 
& nodecnc=[1244,1182,1195,1146,3816,3754,3767,3718,19757, 19758,19759, 19760,19498, 19497,19761, 19762 & 
& ,19763, 19764,19765, 19766,19502, 19501,19767, 19768], & 
& edgecnc=[7307,7308,7177,7309,7310,7311,7179,7312], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1386),elname="xbrick",eltype="xbrick",typekey=1386) 

        call prepare(lib_xbrick(1387),key=1387, & 
& nodecnc=[2362,1147,2395,1131,4934,3719,4967,3703,19730, 19729,19458, 19457,19690, 19689,11058, 11057 & 
& ,19736, 19735,19460, 19459,19692, 19691,11066, 11065], & 
& edgecnc=[7293,7157,7273,2957,7296,7158,7274,2961], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1387),elname="xbrick",eltype="xbrick",typekey=1387) 

        call prepare(lib_xbrick(1388),key=1388, & 
& nodecnc=[160,1161,2540,1174,2732,3733,5112,3746,11086, 11085,19666, 19665,19769, 19770,11106, 11105 & 
& ,11094, 11093,19670, 19669,19771, 19772,11114, 11113], & 
& edgecnc=[2971,7261,7313,2981,2975,7263,7314,2985], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1388),elname="xbrick",eltype="xbrick",typekey=1388) 

        call prepare(lib_xbrick(1389),key=1389, & 
& nodecnc=[1168,1227,1167,2414,3740,3799,3739,4986,8536, 8535,19773, 19774,11194, 11193,19775, 19776,8544 & 
& , 8543,19777, 19778,11200, 11199,19779, 19780], & 
& edgecnc=[1696,7315,3025,7316,1700,7317,3028,7318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1389),elname="xbrick",eltype="xbrick",typekey=1389) 

        call prepare(lib_xbrick(1390),key=1390, & 
& nodecnc=[1240,1148,1151,339,3812,3720,3723,2911,19781, 19782,19638, 19637,19783, 19784,19785, 19786 & 
& ,19787, 19788,19640, 19639,19789, 19790,19791, 19792], & 
& edgecnc=[7319,7247,7320,7321,7322,7248,7323,7324], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1390),elname="xbrick",eltype="xbrick",typekey=1390) 

        call prepare(lib_xbrick(1391),key=1391, & 
& nodecnc=[272,1344,1149,1141,2844,3916,3721,3713,19793, 19794,19795, 19796,8442, 8441,19370, 19369,19797 & 
& , 19798,19799, 19800,8450, 8449,19378, 19377], & 
& edgecnc=[7325,7326,1649,7113,7327,7328,1653,7117], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1391),elname="xbrick",eltype="xbrick",typekey=1391) 

        call prepare(lib_xbrick(1392),key=1392, & 
& nodecnc=[1184,1193,2447,1621,3756,3765,5019,4193,19801, 19802,19803, 19804,19610, 19609,8524, 8523,19805 & 
& , 19806,19807, 19808,19616, 19615,8532, 8531], & 
& edgecnc=[7329,7330,7233,1690,7331,7332,7236,1694], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1392),elname="xbrick",eltype="xbrick",typekey=1392) 

        call prepare(lib_xbrick(1393),key=1393, & 
& nodecnc=[1612,2259,339,1151,4184,4831,2911,3723,19809, 19810,19811, 19812,19784, 19783,19392, 19391 & 
& ,19813, 19814,19815, 19816,19790, 19789,19398, 19397], & 
& edgecnc=[7333,7334,7320,7124,7335,7336,7323,7127], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1393),elname="xbrick",eltype="xbrick",typekey=1393) 

        call prepare(lib_xbrick(1394),key=1394, & 
& nodecnc=[1473,1152,459,70,4045,3724,3031,2642,13766, 13765,8656, 8655,19817, 19818,19819, 19820,13770 & 
& , 13769,8664, 8663,19821, 19822,19823, 19824], & 
& edgecnc=[4311,1756,7337,7338,4313,1760,7339,7340], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1394),elname="xbrick",eltype="xbrick",typekey=1394) 

        call prepare(lib_xbrick(1395),key=1395, & 
& nodecnc=[2259,1612,1705,1802,4831,4184,4277,4374,19810, 19809,19825, 19826,13754, 13753,19827, 19828 & 
& ,19814, 19813,19829, 19830,13760, 13759,19831, 19832], & 
& edgecnc=[7333,7341,4305,7342,7335,7343,4308,7344], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1395),elname="xbrick",eltype="xbrick",typekey=1395) 

        call prepare(lib_xbrick(1396),key=1396, & 
& nodecnc=[1155,1646,71,460,3727,4218,2643,3032,19833, 19834,19414, 19413,19835, 19836,8660, 8659,19837 & 
& , 19838,19420, 19419,19839, 19840,8668, 8667], & 
& edgecnc=[7345,7135,7346,1758,7347,7138,7348,1762], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1396),elname="xbrick",eltype="xbrick",typekey=1396) 

        call prepare(lib_xbrick(1397),key=1397, & 
& nodecnc=[1646,1155,1111,1100,4218,3727,3683,3672,19834, 19833,8672, 8671,19224, 19223,8728, 8727,19838 & 
& , 19837,8678, 8677,19228, 19227,8736, 8735], & 
& edgecnc=[7345,1764,7040,1792,7347,1767,7042,1796], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1397),elname="xbrick",eltype="xbrick",typekey=1397) 

        call prepare(lib_xbrick(1398),key=1398, & 
& nodecnc=[1109,310,1194,1214,3681,2882,3766,3786,19494, 19493,19841, 19842,19843, 19844,19102, 19101 & 
& ,19496, 19495,19845, 19846,19847, 19848,19104, 19103], & 
& edgecnc=[7175,7349,7350,6979,7176,7351,7352,6980], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1398),elname="xbrick",eltype="xbrick",typekey=1398) 

        call prepare(lib_xbrick(1399),key=1399, & 
& nodecnc=[1191,143,1158,1173,3763,2715,3730,3745,19849, 19850,19336, 19335,9760, 9759,19851, 19852,19853 & 
& , 19854,19344, 19343,9768, 9767,19855, 19856], & 
& edgecnc=[7353,7096,2308,7354,7355,7100,2312,7356], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1399),elname="xbrick",eltype="xbrick",typekey=1399) 

        call prepare(lib_xbrick(1400),key=1400, & 
& nodecnc=[1159,1323,72,462,3731,3895,2644,3034,19857, 19858,19859, 19860,19861, 19862,19566, 19565,19863 & 
& , 19864,19865, 19866,19867, 19868,19570, 19569], & 
& edgecnc=[7357,7358,7359,7211,7360,7361,7362,7213], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1400),elname="xbrick",eltype="xbrick",typekey=1400) 

        call prepare(lib_xbrick(1401),key=1401, & 
& nodecnc=[342,2229,1159,1153,2914,4801,3731,3725,19869, 19870,19871, 19872,11252, 11251,19286, 19285 & 
& ,19873, 19874,19875, 19876,11258, 11257,19292, 19291], & 
& edgecnc=[7363,7364,3054,7071,7365,7366,3057,7074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1401),elname="xbrick",eltype="xbrick",typekey=1401) 

        call prepare(lib_xbrick(1402),key=1402, & 
& nodecnc=[1189,1157,1160,1175,3761,3729,3732,3747,5554, 5553,19558, 19557,19562, 19561,19877, 19878,5562 & 
& , 5561,19560, 19559,19564, 19563,19879, 19880], & 
& edgecnc=[205,7207,7209,7367,209,7208,7210,7368], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1402),elname="xbrick",eltype="xbrick",typekey=1402) 

        call prepare(lib_xbrick(1403),key=1403, & 
& nodecnc=[1232,1225,160,1213,3804,3797,2732,3785,19881, 19882,11088, 11087,11104, 11103,19883, 19884 & 
& ,19885, 19886,11096, 11095,11112, 11111,19887, 19888], & 
& edgecnc=[7369,2972,2980,7370,7371,2976,2984,7372], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1403),elname="xbrick",eltype="xbrick",typekey=1403) 

        call prepare(lib_xbrick(1404),key=1404, & 
& nodecnc=[2560,1143,1230,1250,5132,3715,3802,3822,19889, 19890,19738, 19737,11864, 11863,19891, 19892 & 
& ,19893, 19894,19740, 19739,11870, 11869,19895, 19896], & 
& edgecnc=[7373,7297,3360,7374,7375,7298,3363,7376], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1404),elname="xbrick",eltype="xbrick",typekey=1404) 

        call prepare(lib_xbrick(1405),key=1405, & 
& nodecnc=[136,2447,1162,1082,2708,5019,3734,3654,19612, 19611,19897, 19898,19278, 19277,18300, 18299 & 
& ,19618, 19617,19899, 19900,19280, 19279,18306, 18305], & 
& edgecnc=[7234,7377,7067,6578,7237,7378,7068,6581], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1405),elname="xbrick",eltype="xbrick",typekey=1405) 

        call prepare(lib_xbrick(1406),key=1406, & 
& nodecnc=[1215,1194,310,1176,3787,3766,2882,3748,19901, 19902,19842, 19841,8820, 8819,19903, 19904,19905 & 
& , 19906,19846, 19845,8826, 8825,19907, 19908], & 
& edgecnc=[7379,7349,1838,7380,7381,7351,1841,7382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1406),elname="xbrick",eltype="xbrick",typekey=1406) 

        call prepare(lib_xbrick(1407),key=1407, & 
& nodecnc=[1233,1163,1214,1194,3805,3735,3786,3766,19720, 19719,11310, 11309,19844, 19843,19909, 19910 & 
& ,19724, 19723,11318, 11317,19848, 19847,19911, 19912], & 
& edgecnc=[7288,3083,7350,7383,7290,3087,7352,7384], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1407),elname="xbrick",eltype="xbrick",typekey=1407) 

        call prepare(lib_xbrick(1408),key=1408, & 
& nodecnc=[238,1180,1164,1199,2810,3752,3736,3771,19913, 19914,19490, 19489,19915, 19916,19917, 19918 & 
& ,19919, 19920,19492, 19491,19921, 19922,19923, 19924], & 
& edgecnc=[7385,7173,7386,7387,7388,7174,7389,7390], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1408),elname="xbrick",eltype="xbrick",typekey=1408) 

        call prepare(lib_xbrick(1409),key=1409, & 
& nodecnc=[1149,1181,1199,1164,3721,3753,3771,3736,19925, 19926,19927, 19928,19916, 19915,8444, 8443,19929 & 
& , 19930,19931, 19932,19922, 19921,8452, 8451], & 
& edgecnc=[7391,7392,7386,1650,7393,7394,7389,1654], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1409),elname="xbrick",eltype="xbrick",typekey=1409) 

        call prepare(lib_xbrick(1410),key=1410, & 
& nodecnc=[1612,1166,1156,1705,4184,3738,3728,4277,19390, 19389,19402, 19401,8580, 8579,19826, 19825,19396 & 
& , 19395,19404, 19403,8588, 8587,19830, 19829], & 
& edgecnc=[7123,7129,1718,7341,7126,7130,1722,7343], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1410),elname="xbrick",eltype="xbrick",typekey=1410) 

        call prepare(lib_xbrick(1411),key=1411, & 
& nodecnc=[1221,1217,1167,1227,3793,3789,3739,3799,19933, 19934,19935, 19936,19774, 19773,12718, 12717 & 
& ,19937, 19938,19939, 19940,19778, 19777,12724, 12723], & 
& edgecnc=[7395,7396,7315,3787,7397,7398,7317,3790], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1411),elname="xbrick",eltype="xbrick",typekey=1411) 

        call prepare(lib_xbrick(1412),key=1412, & 
& nodecnc=[1240,1168,2414,1148,3812,3740,4986,3720,8538, 8537,19776, 19775,19632, 19631,19782, 19781,8546 & 
& , 8545,19780, 19779,19636, 19635,19788, 19787], & 
& edgecnc=[1697,7316,7244,7319,1701,7318,7246,7322], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1412),elname="xbrick",eltype="xbrick",typekey=1412) 

        call prepare(lib_xbrick(1413),key=1413, & 
& nodecnc=[1665,1815,1814,1732,4237,4387,4386,4304,19642, 19641,19941, 19942,19943, 19944,10410, 10409 & 
& ,19644, 19643,19945, 19946,19947, 19948,10418, 10417], & 
& edgecnc=[7249,7399,7400,2633,7250,7401,7402,2637], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1413),elname="xbrick",eltype="xbrick",typekey=1413) 

        call prepare(lib_xbrick(1414),key=1414, & 
& nodecnc=[1188,243,242,1218,3760,2815,2814,3790,13290, 13289,19648, 19647,19949, 19950,6380, 6379,13292 & 
& , 13291,19652, 19651,19951, 19952,6386, 6385], & 
& edgecnc=[4073,7252,7403,618,4074,7254,7404,621], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1414),elname="xbrick",eltype="xbrick",typekey=1414) 

        call prepare(lib_xbrick(1415),key=1415, & 
& nodecnc=[1144,1185,1202,1171,3716,3757,3774,3743,18940, 18939,19953, 19954,13326, 13325,19744, 19743 & 
& ,18948, 18947,19955, 19956,13330, 13329,19748, 19747], & 
& edgecnc=[6898,7405,4091,7300,6902,7406,4093,7302], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1415),elname="xbrick",eltype="xbrick",typekey=1415) 

        call prepare(lib_xbrick(1416),key=1416, & 
& nodecnc=[1026,2443,1105,1172,3598,5015,3677,3744,8458, 8457,12048, 12047,18910, 18909,18742, 18741,8466 & 
& , 8465,12056, 12055,18916, 18915,18748, 18747], & 
& edgecnc=[1657,3452,6883,6799,1661,3456,6886,6802], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1416),elname="xbrick",eltype="xbrick",typekey=1416) 

        call prepare(lib_xbrick(1417),key=1417, & 
& nodecnc=[2508,1192,1174,1187,5080,3764,3746,3759,19957, 19958,11108, 11107,19959, 19960,19961, 19962 & 
& ,19963, 19964,11116, 11115,19965, 19966,19967, 19968], & 
& edgecnc=[7407,2982,7408,7409,7410,2986,7411,7412], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1417),elname="xbrick",eltype="xbrick",typekey=1417) 

        call prepare(lib_xbrick(1418),key=1418, & 
& nodecnc=[1174,2540,1177,1187,3746,5112,3749,3759,19770, 19769,11146, 11145,19969, 19970,19960, 19959 & 
& ,19772, 19771,11154, 11153,19971, 19972,19966, 19965], & 
& edgecnc=[7313,3001,7413,7408,7314,3005,7414,7411], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1418),elname="xbrick",eltype="xbrick",typekey=1418) 

        call prepare(lib_xbrick(1419),key=1419, & 
& nodecnc=[2330,1200,143,1191,4902,3772,2715,3763,19973, 19974,9746, 9745,19850, 19849,19975, 19976,19977 & 
& , 19978,9754, 9753,19854, 19853,19979, 19980], & 
& edgecnc=[7415,2301,7353,7416,7417,2305,7355,7418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1419),elname="xbrick",eltype="xbrick",typekey=1419) 

        call prepare(lib_xbrick(1420),key=1420, & 
& nodecnc=[1176,1208,1223,1215,3748,3780,3795,3787,5558, 5557,19981, 19982,8804, 8803,19904, 19903,5566 & 
& , 5565,19983, 19984,8812, 8811,19908, 19907], & 
& edgecnc=[207,7419,1830,7380,211,7420,1834,7382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1420),elname="xbrick",eltype="xbrick",typekey=1420) 

        call prepare(lib_xbrick(1421),key=1421, & 
& nodecnc=[311,1285,1208,1189,2883,3857,3780,3761,19985, 19986,19987, 19988,5556, 5555,19989, 19990,19991 & 
& , 19992,19993, 19994,5564, 5563,19995, 19996], & 
& edgecnc=[7421,7422,206,7423,7424,7425,210,7426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1421),elname="xbrick",eltype="xbrick",typekey=1421) 

        call prepare(lib_xbrick(1422),key=1422, & 
& nodecnc=[1236,135,2464,1201,3808,2707,5036,3773,12930, 12929,19997, 19998,19999, 20000,20001, 20002 & 
& ,12938, 12937,20003, 20004,20005, 20006,20007, 20008], & 
& edgecnc=[3893,7427,7428,7429,3897,7430,7431,7432], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1422),elname="xbrick",eltype="xbrick",typekey=1422) 

        call prepare(lib_xbrick(1423),key=1423, & 
& nodecnc=[1136,97,1210,1190,3708,2669,3782,3762,19514, 19513,20009, 20010,20011, 20012,19700, 19699,19518 & 
& , 19517,20013, 20014,20015, 20016,19704, 19703], & 
& edgecnc=[7185,7433,7434,7278,7187,7435,7436,7280], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1423),elname="xbrick",eltype="xbrick",typekey=1423) 

        call prepare(lib_xbrick(1424),key=1424, & 
& nodecnc=[2557,1195,1182,1205,5129,3767,3754,3777,20017, 20018,19760, 19759,20019, 20020,20021, 20022 & 
& ,20023, 20024,19766, 19765,20025, 20026,20027, 20028], & 
& edgecnc=[7437,7308,7438,7439,7440,7311,7441,7442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1424),elname="xbrick",eltype="xbrick",typekey=1424) 

        call prepare(lib_xbrick(1425),key=1425, & 
& nodecnc=[238,1185,1107,1180,2810,3757,3679,3752,20029, 20030,18938, 18937,19126, 19125,19914, 19913 & 
& ,20031, 20032,18946, 18945,19134, 19133,19920, 19919], & 
& edgecnc=[7443,6897,6991,7385,7444,6901,6995,7388], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1425),elname="xbrick",eltype="xbrick",typekey=1425) 

        call prepare(lib_xbrick(1426),key=1426, & 
& nodecnc=[1319,1332,1344,272,3891,3904,3916,2844,19680, 19679,11048, 11047,19794, 19793,19728, 19727 & 
& ,19688, 19687,11056, 11055,19798, 19797,19734, 19733], & 
& edgecnc=[7268,2952,7325,7292,7272,2956,7327,7295], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1426),elname="xbrick",eltype="xbrick",typekey=1426) 

        call prepare(lib_xbrick(1427),key=1427, & 
& nodecnc=[1146,120,1197,1244,3718,2692,3769,3816,9778, 9777,20033, 20034,20035, 20036,19762, 19761,9786 & 
& , 9785,20037, 20038,20039, 20040,19768, 19767], & 
& edgecnc=[2317,7445,7446,7309,2321,7447,7448,7312], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1427),elname="xbrick",eltype="xbrick",typekey=1427) 

        call prepare(lib_xbrick(1428),key=1428, & 
& nodecnc=[119,2567,1182,1244,2691,5139,3754,3816,20041, 20042,20043, 20044,19758, 19757,20045, 20046 & 
& ,20047, 20048,20049, 20050,19764, 19763,20051, 20052], & 
& edgecnc=[7449,7450,7307,7451,7452,7453,7310,7454], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1428),elname="xbrick",eltype="xbrick",typekey=1428) 

        call prepare(lib_xbrick(1429),key=1429, & 
& nodecnc=[400,1186,1203,38,2972,3758,3775,2610,20053, 20054,8410, 8409,8392, 8391,20055, 20056,20057 & 
& , 20058,8418, 8417,8400, 8399,20059, 20060], & 
& edgecnc=[7455,1633,1624,7456,7457,1637,1628,7458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1429),elname="xbrick",eltype="xbrick",typekey=1429) 

        call prepare(lib_xbrick(1430),key=1430, & 
& nodecnc=[1209,1322,1192,2508,3781,3894,3764,5080,11118, 11117,20061, 20062,19958, 19957,20063, 20064 & 
& ,11126, 11125,20065, 20066,19964, 19963,20067, 20068], & 
& edgecnc=[2987,7459,7407,7460,2991,7461,7410,7462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1430),elname="xbrick",eltype="xbrick",typekey=1430) 

        call prepare(lib_xbrick(1431),key=1431, & 
& nodecnc=[1193,135,1187,1177,3765,2707,3759,3749,20069, 20070,20071, 20072,19970, 19969,20073, 20074 & 
& ,20075, 20076,20077, 20078,19972, 19971,20079, 20080], & 
& edgecnc=[7463,7464,7413,7465,7466,7467,7414,7468], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1431),elname="xbrick",eltype="xbrick",typekey=1431) 

        call prepare(lib_xbrick(1432),key=1432, & 
& nodecnc=[1200,311,1189,1175,3772,2883,3761,3747,20081, 20082,19990, 19989,19878, 19877,9748, 9747,20083 & 
& , 20084,19996, 19995,19880, 19879,9756, 9755], & 
& edgecnc=[7469,7423,7367,2302,7470,7426,7368,2306], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1432),elname="xbrick",eltype="xbrick",typekey=1432) 

        call prepare(lib_xbrick(1433),key=1433, & 
& nodecnc=[2501,1212,1173,1211,5073,3784,3745,3783,20085, 20086,20087, 20088,9758, 9757,9822, 9821,20089 & 
& , 20090,20091, 20092,9766, 9765,9830, 9829], & 
& edgecnc=[7471,7472,2307,2339,7473,7474,2311,2343], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1433),elname="xbrick",eltype="xbrick",typekey=1433) 

        call prepare(lib_xbrick(1434),key=1434, & 
& nodecnc=[1173,1212,1224,1191,3745,3784,3796,3763,20088, 20087,20093, 20094,20095, 20096,19852, 19851 & 
& ,20092, 20091,20097, 20098,20099, 20100,19856, 19855], & 
& edgecnc=[7472,7475,7476,7354,7474,7477,7478,7356], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1434),elname="xbrick",eltype="xbrick",typekey=1434) 

        call prepare(lib_xbrick(1435),key=1435, & 
& nodecnc=[1280,311,1200,2330,3852,2883,3772,4902,20101, 20102,20082, 20081,19974, 19973,20103, 20104 & 
& ,20105, 20106,20084, 20083,19978, 19977,20107, 20108], & 
& edgecnc=[7479,7469,7415,7480,7481,7470,7417,7482], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1435),elname="xbrick",eltype="xbrick",typekey=1435) 

        call prepare(lib_xbrick(1436),key=1436, & 
& nodecnc=[1162,2447,1193,1177,3734,5019,3765,3749,19898, 19897,19804, 19803,20074, 20073,11152, 11151 & 
& ,19900, 19899,19808, 19807,20080, 20079,11160, 11159], & 
& edgecnc=[7377,7330,7465,3004,7378,7332,7468,3008], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1436),elname="xbrick",eltype="xbrick",typekey=1436) 

        call prepare(lib_xbrick(1437),key=1437, & 
& nodecnc=[2557,1207,1178,1195,5129,3779,3750,3767,20109, 20110,20111, 20112,19500, 19499,20018, 20017 & 
& ,20113, 20114,20115, 20116,19504, 19503,20024, 20023], & 
& edgecnc=[7483,7484,7178,7437,7485,7486,7180,7440], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1437),elname="xbrick",eltype="xbrick",typekey=1437) 

        call prepare(lib_xbrick(1438),key=1438, & 
& nodecnc=[1197,120,1196,1752,3769,2692,3768,4324,20034, 20033,19382, 19381,20117, 20118,9792, 9791,20038 & 
& , 20037,19384, 19383,20119, 20120,9800, 9799], & 
& edgecnc=[7445,7119,7487,2324,7447,7120,7488,2328], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1438),elname="xbrick",eltype="xbrick",typekey=1438) 

        call prepare(lib_xbrick(1439),key=1439, & 
& nodecnc=[1819,1814,1815,1198,4391,4386,4387,3770,20121, 20122,19942, 19941,12370, 12369,20123, 20124 & 
& ,20125, 20126,19946, 19945,12376, 12375,20127, 20128], & 
& edgecnc=[7489,7399,3613,7490,7491,7401,3616,7492], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1439),elname="xbrick",eltype="xbrick",typekey=1439) 

        call prepare(lib_xbrick(1440),key=1440, & 
& nodecnc=[1392,1216,238,1199,3964,3788,2810,3771,20129, 20130,20131, 20132,19918, 19917,20133, 20134 & 
& ,20135, 20136,20137, 20138,19924, 19923,20139, 20140], & 
& edgecnc=[7493,7494,7387,7495,7496,7497,7390,7498], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1440),elname="xbrick",eltype="xbrick",typekey=1440) 

        call prepare(lib_xbrick(1441),key=1441, & 
& nodecnc=[1392,1199,1181,237,3964,3771,3753,2809,20134, 20133,19928, 19927,20141, 20142,8428, 8427,20140 & 
& , 20139,19932, 19931,20143, 20144,8436, 8435], & 
& edgecnc=[7495,7392,7499,1642,7498,7394,7500,1646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1441),elname="xbrick",eltype="xbrick",typekey=1441) 

        call prepare(lib_xbrick(1442),key=1442, & 
& nodecnc=[1273,1280,2330,1252,3845,3852,4902,3824,20145, 20146,20104, 20103,20147, 20148,12762, 12761 & 
& ,20149, 20150,20108, 20107,20151, 20152,12770, 12769], & 
& edgecnc=[7501,7480,7502,3809,7503,7482,7504,3813], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1442),elname="xbrick",eltype="xbrick",typekey=1442) 

        call prepare(lib_xbrick(1443),key=1443, & 
& nodecnc=[1201,2464,1217,304,3773,5036,3789,2876,20000, 19999,20153, 20154,20155, 20156,20157, 20158 & 
& ,20006, 20005,20159, 20160,20161, 20162,20163, 20164], & 
& edgecnc=[7428,7505,7506,7507,7431,7508,7509,7510], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1443),elname="xbrick",eltype="xbrick",typekey=1443) 

        call prepare(lib_xbrick(1444),key=1444, & 
& nodecnc=[1216,1202,1185,238,3788,3774,3757,2810,13336, 13335,19954, 19953,20030, 20029,20132, 20131 & 
& ,13342, 13341,19956, 19955,20032, 20031,20138, 20137], & 
& edgecnc=[4096,7405,7443,7494,4099,7406,7444,7497], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1444),elname="xbrick",eltype="xbrick",typekey=1444) 

        call prepare(lib_xbrick(1445),key=1445, & 
& nodecnc=[2235,239,1204,1618,4807,2811,3776,4190,19742, 19741,13320, 13319,19170, 19169,20165, 20166 & 
& ,19746, 19745,13324, 13323,19176, 19175,20167, 20168], & 
& edgecnc=[7299,4088,7013,7511,7301,4090,7016,7512], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1445),elname="xbrick",eltype="xbrick",typekey=1445) 

        call prepare(lib_xbrick(1446),key=1446, & 
& nodecnc=[2555,1359,1367,1381,5127,3931,3939,3953,20169, 20170,20171, 20172,9898, 9897,20173, 20174,20175 & 
& , 20176,20177, 20178,9906, 9905,20179, 20180], & 
& edgecnc=[7513,7514,2377,7515,7516,7517,2381,7518], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1446),elname="xbrick",eltype="xbrick",typekey=1446) 

        call prepare(lib_xbrick(1447),key=1447, & 
& nodecnc=[2567,2565,1205,1182,5139,5137,3777,3754,20181, 20182,20183, 20184,20020, 20019,20044, 20043 & 
& ,20185, 20186,20187, 20188,20026, 20025,20050, 20049], & 
& edgecnc=[7519,7520,7438,7450,7521,7522,7441,7453], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1447),elname="xbrick",eltype="xbrick",typekey=1447) 

        call prepare(lib_xbrick(1448),key=1448, & 
& nodecnc=[1752,1196,1086,1206,4324,3768,3658,3778,20118, 20117,19322, 19321,10396, 10395,20189, 20190 & 
& ,20120, 20119,19328, 19327,10404, 10403,20191, 20192], & 
& edgecnc=[7487,7089,2626,7523,7488,7092,2630,7524], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1448),elname="xbrick",eltype="xbrick",typekey=1448) 

        call prepare(lib_xbrick(1449),key=1449, & 
& nodecnc=[1752,1206,1663,204,4324,3778,4235,2776,20190, 20189,12022, 12021,20193, 20194,19198, 19197 & 
& ,20192, 20191,12026, 12025,20195, 20196,19204, 19203], & 
& edgecnc=[7523,3439,7525,7027,7524,3441,7526,7030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1449),elname="xbrick",eltype="xbrick",typekey=1449) 

        call prepare(lib_xbrick(1450),key=1450, & 
& nodecnc=[97,1178,1207,1210,2669,3750,3779,3782,19446, 19445,20112, 20111,20197, 20198,20010, 20009,19452 & 
& , 19451,20116, 20115,20199, 20200,20014, 20013], & 
& edgecnc=[7151,7484,7527,7433,7154,7486,7528,7435], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1450),elname="xbrick",eltype="xbrick",typekey=1450) 

        call prepare(lib_xbrick(1451),key=1451, & 
& nodecnc=[1285,1235,1223,1208,3857,3807,3795,3780,20201, 20202,13816, 13815,19982, 19981,19988, 19987 & 
& ,20203, 20204,13820, 13819,19984, 19983,19994, 19993], & 
& edgecnc=[7529,4336,7419,7422,7530,4338,7420,7425], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1451),elname="xbrick",eltype="xbrick",typekey=1451) 

        call prepare(lib_xbrick(1452),key=1452, & 
& nodecnc=[2381,1241,1209,1222,4953,3813,3781,3794,20205, 20206,11120, 11119,20207, 20208,20209, 20210 & 
& ,20211, 20212,11128, 11127,20213, 20214,20215, 20216], & 
& edgecnc=[7531,2988,7532,7533,7534,2992,7535,7536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1452),elname="xbrick",eltype="xbrick",typekey=1452) 

        call prepare(lib_xbrick(1453),key=1453, & 
& nodecnc=[135,2483,2508,1187,2707,5055,5080,3759,12928, 12927,20217, 20218,19962, 19961,20072, 20071 & 
& ,12936, 12935,20219, 20220,19968, 19967,20078, 20077], & 
& edgecnc=[3892,7537,7409,7464,3896,7538,7412,7467], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1453),elname="xbrick",eltype="xbrick",typekey=1453) 

        call prepare(lib_xbrick(1454),key=1454, & 
& nodecnc=[2539,1211,1190,1210,5111,3783,3762,3782,9824, 9823,9764, 9763,20012, 20011,20221, 20222,9832 & 
& , 9831,9772, 9771,20016, 20015,20223, 20224], & 
& edgecnc=[2340,2310,7434,7539,2344,2314,7436,7540], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1454),elname="xbrick",eltype="xbrick",typekey=1454) 

        call prepare(lib_xbrick(1455),key=1455, & 
& nodecnc=[1220,2532,1231,2501,3792,5104,3803,5073,20225, 20226,20227, 20228,20229, 20230,9828, 9827,20231 & 
& , 20232,20233, 20234,20235, 20236,9836, 9835], & 
& edgecnc=[7541,7542,7543,2342,7544,7545,7546,2346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1455),elname="xbrick",eltype="xbrick",typekey=1455) 

        call prepare(lib_xbrick(1456),key=1456, & 
& nodecnc=[144,1212,2501,1231,2716,3784,5073,3803,20237, 20238,20086, 20085,20230, 20229,11344, 11343 & 
& ,20239, 20240,20090, 20089,20236, 20235,11350, 11349], & 
& edgecnc=[7547,7471,7543,3100,7548,7473,7546,3103], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1456),elname="xbrick",eltype="xbrick",typekey=1456) 

        call prepare(lib_xbrick(1457),key=1457, & 
& nodecnc=[161,1264,2528,1232,2733,3836,5100,3804,11956, 11955,20241, 20242,20243, 20244,20245, 20246 & 
& ,11964, 11963,20247, 20248,20249, 20250,20251, 20252], & 
& edgecnc=[3406,7549,7550,7551,3410,7552,7553,7554], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1457),elname="xbrick",eltype="xbrick",typekey=1457) 

        call prepare(lib_xbrick(1458),key=1458, & 
& nodecnc=[1232,1213,1226,161,3804,3785,3798,2733,19884, 19883,20253, 20254,20255, 20256,20246, 20245 & 
& ,19888, 19887,20257, 20258,20259, 20260,20252, 20251], & 
& edgecnc=[7370,7555,7556,7551,7372,7557,7558,7554], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1458),elname="xbrick",eltype="xbrick",typekey=1458) 

        call prepare(lib_xbrick(1459),key=1459, & 
& nodecnc=[344,1233,1194,1215,2916,3805,3766,3787,20261, 20262,19910, 19909,19902, 19901,8802, 8801,20263 & 
& , 20264,19912, 19911,19906, 19905,8810, 8809], & 
& edgecnc=[7559,7383,7379,1829,7560,7384,7381,1833], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1459),elname="xbrick",eltype="xbrick",typekey=1459) 

        call prepare(lib_xbrick(1460),key=1460, & 
& nodecnc=[1622,2207,1216,1392,4194,4779,3788,3964,20265, 20266,13338, 13337,20130, 20129,8426, 8425,20267 & 
& , 20268,13344, 13343,20136, 20135,8434, 8433], & 
& edgecnc=[7561,4097,7493,1641,7562,4100,7496,1645], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1460),elname="xbrick",eltype="xbrick",typekey=1460) 

        call prepare(lib_xbrick(1461),key=1461, & 
& nodecnc=[135,1193,1184,2464,2707,3765,3756,5036,20070, 20069,19802, 19801,20269, 20270,19998, 19997 & 
& ,20076, 20075,19806, 19805,20271, 20272,20004, 20003], & 
& edgecnc=[7463,7329,7563,7427,7466,7331,7564,7430], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1461),elname="xbrick",eltype="xbrick",typekey=1461) 

        call prepare(lib_xbrick(1462),key=1462, & 
& nodecnc=[1184,1167,1217,2464,3756,3739,3789,5036,8522, 8521,19936, 19935,20154, 20153,20270, 20269,8530 & 
& , 8529,19940, 19939,20160, 20159,20272, 20271], & 
& edgecnc=[1689,7396,7505,7563,1693,7398,7508,7564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1462),elname="xbrick",eltype="xbrick",typekey=1462) 

        call prepare(lib_xbrick(1463),key=1463, & 
& nodecnc=[1239,304,1217,1221,3811,2876,3789,3793,20273, 20274,20156, 20155,19934, 19933,20275, 20276 & 
& ,20277, 20278,20162, 20161,19938, 19937,20279, 20280], & 
& edgecnc=[7565,7506,7395,7566,7567,7509,7397,7568], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1463),elname="xbrick",eltype="xbrick",typekey=1463) 

        call prepare(lib_xbrick(1464),key=1464, & 
& nodecnc=[1218,1501,396,395,3790,4073,2968,2967,20281, 20282,13294, 13293,20283, 20284,6382, 6381,20285 & 
& , 20286,13298, 13297,20287, 20288,6388, 6387], & 
& edgecnc=[7569,4075,7570,619,7571,4077,7572,622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1464),elname="xbrick",eltype="xbrick",typekey=1464) 

        call prepare(lib_xbrick(1465),key=1465, & 
& nodecnc=[1210,1207,1220,2539,3782,3779,3792,5111,20198, 20197,20289, 20290,9826, 9825,20222, 20221,20200 & 
& , 20199,20291, 20292,9834, 9833,20224, 20223], & 
& edgecnc=[7527,7573,2341,7539,7528,7574,2345,7540], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1465),elname="xbrick",eltype="xbrick",typekey=1465) 

        call prepare(lib_xbrick(1466),key=1466, & 
& nodecnc=[1254,1241,2381,1242,3826,3813,4953,3814,20293, 20294,20206, 20205,20295, 20296,20297, 20298 & 
& ,20299, 20300,20212, 20211,20301, 20302,20303, 20304], & 
& edgecnc=[7575,7531,7576,7577,7578,7534,7579,7580], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1466),elname="xbrick",eltype="xbrick",typekey=1466) 

        call prepare(lib_xbrick(1467),key=1467, & 
& nodecnc=[1261,1259,345,1258,3833,3831,2917,3830,20305, 20306,13814, 13813,13824, 13823,20307, 20308 & 
& ,20309, 20310,13818, 13817,13828, 13827,20311, 20312], & 
& edgecnc=[7581,4335,4340,7582,7583,4337,4342,7584], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1467),elname="xbrick",eltype="xbrick",typekey=1467) 

        call prepare(lib_xbrick(1468),key=1468, & 
& nodecnc=[1247,1252,1224,1234,3819,3824,3796,3806,12764, 12763,20313, 20314,20315, 20316,12778, 12777 & 
& ,12772, 12771,20317, 20318,20319, 20320,12784, 12783], & 
& edgecnc=[3810,7585,7586,3817,3814,7587,7588,3820], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1468),elname="xbrick",eltype="xbrick",typekey=1468) 

        call prepare(lib_xbrick(1469),key=1469, & 
& nodecnc=[1234,1224,1212,144,3806,3796,3784,2716,20316, 20315,20094, 20093,20238, 20237,9888, 9887,20320 & 
& , 20319,20098, 20097,20240, 20239,9896, 9895], & 
& edgecnc=[7586,7475,7547,2372,7588,7477,7548,2376], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1469),elname="xbrick",eltype="xbrick",typekey=1469) 

        call prepare(lib_xbrick(1470),key=1470, & 
& nodecnc=[2528,1243,1225,1232,5100,3815,3797,3804,20321, 20322,11866, 11865,19882, 19881,20244, 20243 & 
& ,20323, 20324,11872, 11871,19886, 19885,20250, 20249], & 
& edgecnc=[7589,3361,7369,7550,7590,3364,7371,7553], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1470),elname="xbrick",eltype="xbrick",typekey=1470) 

        call prepare(lib_xbrick(1471),key=1471, & 
& nodecnc=[1322,1226,1213,1192,3894,3798,3785,3764,20325, 20326,20254, 20253,11102, 11101,20062, 20061 & 
& ,20327, 20328,20258, 20257,11110, 11109,20066, 20065], & 
& edgecnc=[7591,7555,2979,7459,7592,7557,2983,7461], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1471),elname="xbrick",eltype="xbrick",typekey=1471) 

        call prepare(lib_xbrick(1472),key=1472, & 
& nodecnc=[1226,1322,1249,2502,3798,3894,3821,5074,20326, 20325,11124, 11123,20329, 20330,20331, 20332 & 
& ,20328, 20327,11132, 11131,20333, 20334,20335, 20336], & 
& edgecnc=[7591,2990,7593,7594,7592,2994,7595,7596], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1472),elname="xbrick",eltype="xbrick",typekey=1472) 

        call prepare(lib_xbrick(1473),key=1473, & 
& nodecnc=[119,1244,1197,1238,2691,3816,3769,3810,20046, 20045,20036, 20035,9790, 9789,9914, 9913,20052 & 
& , 20051,20040, 20039,9798, 9797,9922, 9921], & 
& edgecnc=[7451,7446,2323,2385,7454,7448,2327,2389], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1473),elname="xbrick",eltype="xbrick",typekey=1473) 

        call prepare(lib_xbrick(1474),key=1474, & 
& nodecnc=[1615,203,1228,1237,4187,2775,3800,3809,9934, 9933,20337, 20338,19202, 19201,9812, 9811,9942 & 
& , 9941,20339, 20340,19208, 19207,9820, 9819], & 
& edgecnc=[2395,7597,7029,2334,2399,7598,7032,2338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1474),elname="xbrick",eltype="xbrick",typekey=1474) 

        call prepare(lib_xbrick(1475),key=1475, & 
& nodecnc=[402,401,1229,2207,2974,2973,3801,4779,20341, 20342,8396, 8395,13334, 13333,20343, 20344,20345 & 
& , 20346,8404, 8403,13340, 13339,20347, 20348], & 
& edgecnc=[7599,1626,4095,7600,7601,1630,4098,7602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1475),elname="xbrick",eltype="xbrick",typekey=1475) 

        call prepare(lib_xbrick(1476),key=1476, & 
& nodecnc=[145,1246,1631,1325,2717,3818,4203,3897,20349, 20350,11342, 11341,20351, 20352,20353, 20354 & 
& ,20355, 20356,11348, 11347,20357, 20358,20359, 20360], & 
& edgecnc=[7603,3099,7604,7605,7606,3102,7607,7608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1476),elname="xbrick",eltype="xbrick",typekey=1476) 

        call prepare(lib_xbrick(1477),key=1477, & 
& nodecnc=[98,1631,1231,2532,2670,4203,3803,5104,9902, 9901,11346, 11345,20228, 20227,20361, 20362,9910 & 
& , 9909,11352, 11351,20234, 20233,20363, 20364], & 
& edgecnc=[2379,3101,7542,7609,2383,3104,7545,7610], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1477),elname="xbrick",eltype="xbrick",typekey=1477) 

        call prepare(lib_xbrick(1478),key=1478, & 
& nodecnc=[344,1282,1179,1233,2916,3854,3751,3805,20365, 20366,20367, 20368,19718, 19717,20262, 20261 & 
& ,20369, 20370,20371, 20372,19722, 19721,20264, 20263], & 
& edgecnc=[7611,7612,7287,7559,7613,7614,7289,7560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1478),elname="xbrick",eltype="xbrick",typekey=1478) 

        call prepare(lib_xbrick(1479),key=1479, & 
& nodecnc=[1302,1235,1285,1256,3874,3807,3857,3828,9728, 9727,20202, 20201,20373, 20374,20375, 20376,9736 & 
& , 9735,20204, 20203,20377, 20378,20379, 20380], & 
& edgecnc=[2292,7529,7615,7616,2296,7530,7617,7618], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1479),elname="xbrick",eltype="xbrick",typekey=1479) 

        call prepare(lib_xbrick(1480),key=1480, & 
& nodecnc=[1222,1236,1649,2381,3794,3808,4221,4953,12932, 12931,20381, 20382,20383, 20384,20210, 20209 & 
& ,12940, 12939,20385, 20386,20387, 20388,20216, 20215], & 
& edgecnc=[3894,7619,7620,7533,3898,7621,7622,7536], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1480),elname="xbrick",eltype="xbrick",typekey=1480) 

        call prepare(lib_xbrick(1481),key=1481, & 
& nodecnc=[1649,1236,1201,304,4221,3808,3773,2876,20382, 20381,20002, 20001,20158, 20157,20389, 20390 & 
& ,20386, 20385,20008, 20007,20164, 20163,20391, 20392], & 
& edgecnc=[7619,7429,7507,7623,7621,7432,7510,7624], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1481),elname="xbrick",eltype="xbrick",typekey=1481) 

        call prepare(lib_xbrick(1482),key=1482, & 
& nodecnc=[203,1639,1238,1228,2775,4211,3810,3800,20393, 20394,9916, 9915,9796, 9795,20338, 20337,20395 & 
& , 20396,9924, 9923,9804, 9803,20340, 20339], & 
& edgecnc=[7625,2386,2326,7597,7626,2390,2330,7598], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1482),elname="xbrick",eltype="xbrick",typekey=1482) 

        call prepare(lib_xbrick(1483),key=1483, & 
& nodecnc=[2329,1239,1423,1430,4901,3811,3995,4002,20397, 20398,20399, 20400,11138, 11137,20401, 20402 & 
& ,20403, 20404,20405, 20406,11144, 11143,20407, 20408], & 
& edgecnc=[7627,7628,2997,7629,7630,7631,3000,7632], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1483),elname="xbrick",eltype="xbrick",typekey=1483) 

        call prepare(lib_xbrick(1484),key=1484, & 
& nodecnc=[1249,1241,1254,134,3821,3813,3826,2706,11122, 11121,20294, 20293,10866, 10865,20409, 20410 & 
& ,11130, 11129,20300, 20299,10874, 10873,20411, 20412], & 
& edgecnc=[2989,7575,2861,7633,2993,7578,2865,7634], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1484),elname="xbrick",eltype="xbrick",typekey=1484) 

        call prepare(lib_xbrick(1485),key=1485, & 
& nodecnc=[1430,303,1242,2329,4002,2875,3814,4901,20413, 20414,20415, 20416,20417, 20418,20402, 20401 & 
& ,20419, 20420,20421, 20422,20423, 20424,20408, 20407], & 
& edgecnc=[7635,7636,7637,7629,7638,7639,7640,7632], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1485),elname="xbrick",eltype="xbrick",typekey=1485) 

        call prepare(lib_xbrick(1486),key=1486, & 
& nodecnc=[346,1302,1256,1267,2918,3874,3828,3839,20425, 20426,20376, 20375,20427, 20428,20429, 20430 & 
& ,20431, 20432,20380, 20379,20433, 20434,20435, 20436], & 
& edgecnc=[7641,7616,7642,7643,7644,7618,7645,7646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1486),elname="xbrick",eltype="xbrick",typekey=1486) 

        call prepare(lib_xbrick(1487),key=1487, & 
& nodecnc=[1243,2528,1270,1263,3815,5100,3842,3835,20322, 20321,20437, 20438,20439, 20440,11074, 11073 & 
& ,20324, 20323,20441, 20442,20443, 20444,11080, 11079], & 
& edgecnc=[7589,7647,7648,2965,7590,7649,7650,2968], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1487),elname="xbrick",eltype="xbrick",typekey=1487) 

        call prepare(lib_xbrick(1488),key=1488, & 
& nodecnc=[1255,2567,119,1245,3827,5139,2691,3817,20445, 20446,20042, 20041,9920, 9919,10350, 10349,20447 & 
& , 20448,20048, 20047,9928, 9927,10358, 10357], & 
& edgecnc=[7651,7449,2388,2603,7652,7452,2392,2607], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1488),elname="xbrick",eltype="xbrick",typekey=1488) 

        call prepare(lib_xbrick(1489),key=1489, & 
& nodecnc=[1750,1664,1245,1639,4322,4236,3817,4211,20449, 20450,10352, 10351,9918, 9917,20451, 20452,20453 & 
& , 20454,10360, 10359,9926, 9925,20455, 20456], & 
& edgecnc=[7653,2604,2387,7654,7655,2608,2391,7656], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1489),elname="xbrick",eltype="xbrick",typekey=1489) 

        call prepare(lib_xbrick(1490),key=1490, & 
& nodecnc=[1247,1253,1284,312,3819,3825,3856,2884,12782, 12781,9870, 9869,9850, 9849,12766, 12765,12788 & 
& , 12787,9878, 9877,9858, 9857,12774, 12773], & 
& edgecnc=[3819,2363,2353,3811,3822,2367,2357,3815], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1490),elname="xbrick",eltype="xbrick",typekey=1490) 

        call prepare(lib_xbrick(1491),key=1491, & 
& nodecnc=[1260,1268,338,1248,3832,3840,2910,3820,20457, 20458,10902, 10901,12720, 12719,20459, 20460 & 
& ,20461, 20462,10908, 10907,12726, 12725,20463, 20464], & 
& edgecnc=[7657,2879,3788,7658,7659,2882,3791,7660], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1491),elname="xbrick",eltype="xbrick",typekey=1491) 

        call prepare(lib_xbrick(1492),key=1492, & 
& nodecnc=[1260,1248,1240,1748,3832,3820,3812,4320,20460, 20459,8540, 8539,20465, 20466,8234, 8233,20464 & 
& , 20463,8548, 8547,20467, 20468,8242, 8241], & 
& edgecnc=[7658,1698,7661,1545,7660,1702,7662,1549], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1492),elname="xbrick",eltype="xbrick",typekey=1492) 

        call prepare(lib_xbrick(1493),key=1493, & 
& nodecnc=[465,1261,1258,466,3037,3833,3830,3038,8790, 8789,20308, 20307,20469, 20470,20471, 20472,8798 & 
& , 8797,20312, 20311,20473, 20474,20475, 20476], & 
& edgecnc=[1823,7582,7663,7664,1827,7584,7665,7666], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1493),elname="xbrick",eltype="xbrick",typekey=1493) 

        call prepare(lib_xbrick(1494),key=1494, & 
& nodecnc=[1502,74,466,1258,4074,2646,3038,3830,20477, 20478,20479, 20480,20470, 20469,13822, 13821,20481 & 
& , 20482,20483, 20484,20474, 20473,13826, 13825], & 
& edgecnc=[7667,7668,7663,4339,7669,7670,7665,4341], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1494),elname="xbrick",eltype="xbrick",typekey=1494) 

        call prepare(lib_xbrick(1495),key=1495, & 
& nodecnc=[1265,2502,1249,134,3837,5074,3821,2706,20485, 20486,20330, 20329,20410, 20409,10926, 10925 & 
& ,20487, 20488,20334, 20333,20412, 20411,10932, 10931], & 
& edgecnc=[7671,7593,7633,2891,7672,7595,7634,2894], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1495),elname="xbrick",eltype="xbrick",typekey=1495) 

        call prepare(lib_xbrick(1496),key=1496, & 
& nodecnc=[1295,1283,1265,1271,3867,3855,3837,3843,10952, 10951,20489, 20490,10936, 10935,20491, 20492 & 
& ,10960, 10959,20493, 20494,10942, 10941,20495, 20496], & 
& edgecnc=[2904,7673,2896,7674,2908,7675,2899,7676], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1496),elname="xbrick",eltype="xbrick",typekey=1496) 

        call prepare(lib_xbrick(1497),key=1497, & 
& nodecnc=[1128,2560,1250,1137,3700,5132,3822,3709,20497, 20498,19892, 19891,19656, 19655,19474, 19473 & 
& ,20499, 20500,19896, 19895,19660, 19659,19478, 19477], & 
& edgecnc=[7677,7374,7256,7165,7678,7376,7258,7167], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1497),elname="xbrick",eltype="xbrick",typekey=1497) 

        call prepare(lib_xbrick(1498),key=1498, & 
& nodecnc=[1251,1294,1266,1253,3823,3866,3838,3825,20501, 20502,20503, 20504,9872, 9871,12780, 12779,20505 & 
& , 20506,20507, 20508,9880, 9879,12786, 12785], & 
& edgecnc=[7679,7680,2364,3818,7681,7682,2368,3821], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1498),elname="xbrick",eltype="xbrick",typekey=1498) 

        call prepare(lib_xbrick(1499),key=1499, & 
& nodecnc=[1294,1251,1246,145,3866,3823,3818,2717,20502, 20501,9884, 9883,20350, 20349,11354, 11353,20506 & 
& , 20505,9892, 9891,20356, 20355,11360, 11359], & 
& edgecnc=[7679,2370,7603,3105,7681,2374,7606,3108], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1499),elname="xbrick",eltype="xbrick",typekey=1499) 

        call prepare(lib_xbrick(1500),key=1500, & 
& nodecnc=[2330,1191,1224,1252,4902,3763,3796,3824,19976, 19975,20096, 20095,20314, 20313,20148, 20147 & 
& ,19980, 19979,20100, 20099,20318, 20317,20152, 20151], & 
& edgecnc=[7416,7476,7585,7502,7418,7478,7587,7504], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1500),elname="xbrick",eltype="xbrick",typekey=1500) 

        call prepare(lib_xbrick(1501),key=1501, & 
& nodecnc=[1267,1256,1280,1273,3839,3828,3852,3845,20428, 20427,20509, 20510,20146, 20145,20511, 20512 & 
& ,20434, 20433,20513, 20514,20150, 20149,20515, 20516], & 
& edgecnc=[7642,7683,7501,7684,7645,7685,7503,7686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1501),elname="xbrick",eltype="xbrick",typekey=1501) 

        call prepare(lib_xbrick(1502),key=1502, & 
& nodecnc=[1290,1254,1242,303,3862,3826,3814,2875,10868, 10867,20298, 20297,20416, 20415,20517, 20518 & 
& ,10876, 10875,20304, 20303,20422, 20421,20519, 20520], & 
& edgecnc=[2862,7577,7636,7687,2866,7580,7639,7688], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1502),elname="xbrick",eltype="xbrick",typekey=1502) 

        call prepare(lib_xbrick(1503),key=1503, & 
& nodecnc=[1882,2555,1381,1255,4454,5127,3953,3827,20521, 20522,20174, 20173,20523, 20524,10356, 10355 & 
& ,20525, 20526,20180, 20179,20527, 20528,10364, 10363], & 
& edgecnc=[7689,7515,7690,2606,7691,7518,7692,2610], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1503),elname="xbrick",eltype="xbrick",typekey=1503) 

        call prepare(lib_xbrick(1504),key=1504, & 
& nodecnc=[1286,1267,1273,312,3858,3839,3845,2884,20529, 20530,20512, 20511,12768, 12767,9856, 9855,20531 & 
& , 20532,20516, 20515,12776, 12775,9864, 9863], & 
& edgecnc=[7693,7684,3812,2356,7694,7686,3816,2360], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1504),elname="xbrick",eltype="xbrick",typekey=1504) 

        call prepare(lib_xbrick(1505),key=1505, & 
& nodecnc=[1280,1256,1285,311,3852,3828,3857,2883,20510, 20509,20374, 20373,19986, 19985,20102, 20101 & 
& ,20514, 20513,20378, 20377,19992, 19991,20106, 20105], & 
& edgecnc=[7683,7615,7421,7479,7685,7617,7424,7481], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1505),elname="xbrick",eltype="xbrick",typekey=1505) 

        call prepare(lib_xbrick(1506),key=1506, & 
& nodecnc=[344,1259,1261,1262,2916,3831,3833,3834,8808, 8807,20306, 20305,8788, 8787,20533, 20534,8816 & 
& , 8815,20310, 20309,8796, 8795,20535, 20536], & 
& edgecnc=[1832,7581,1822,7695,1836,7583,1826,7696], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1506),elname="xbrick",eltype="xbrick",typekey=1506) 

        call prepare(lib_xbrick(1507),key=1507, & 
& nodecnc=[1636,1729,456,455,4208,4301,3028,3027,20537, 20538,20539, 20540,20541, 20542,13748, 13747,20543 & 
& , 20544,20545, 20546,20547, 20548,13752, 13751], & 
& edgecnc=[7697,7698,7699,4302,7700,7701,7702,4304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1507),elname="xbrick",eltype="xbrick",typekey=1507) 

        call prepare(lib_xbrick(1508),key=1508, & 
& nodecnc=[1276,1275,1263,1270,3848,3847,3835,3842,20549, 20550,10966, 10965,20440, 20439,20551, 20552 & 
& ,20553, 20554,10974, 10973,20444, 20443,20555, 20556], & 
& edgecnc=[7703,2911,7648,7704,7705,2915,7650,7706], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1508),elname="xbrick",eltype="xbrick",typekey=1508) 

        call prepare(lib_xbrick(1509),key=1509, & 
& nodecnc=[1264,178,1270,2528,3836,2750,3842,5100,20557, 20558,20559, 20560,20438, 20437,20242, 20241 & 
& ,20561, 20562,20563, 20564,20442, 20441,20248, 20247], & 
& edgecnc=[7707,7708,7647,7549,7709,7710,7649,7552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1509),elname="xbrick",eltype="xbrick",typekey=1509) 

        call prepare(lib_xbrick(1510),key=1510, & 
& nodecnc=[1657,1307,2524,1283,4229,3879,5096,3855,20565, 20566,11952, 11951,20567, 20568,10950, 10949 & 
& ,20569, 20570,11960, 11959,20571, 20572,10958, 10957], & 
& edgecnc=[7711,3404,7712,2903,7713,3408,7714,2907], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1510),elname="xbrick",eltype="xbrick",typekey=1510) 

        call prepare(lib_xbrick(1511),key=1511, & 
& nodecnc=[1278,1303,1293,313,3850,3875,3865,2885,5646, 5645,20573, 20574,20575, 20576,12792, 12791,5654 & 
& , 5653,20577, 20578,20579, 20580,12798, 12797], & 
& edgecnc=[251,7715,7716,3824,255,7717,7718,3827], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1511),elname="xbrick",eltype="xbrick",typekey=1511) 

        call prepare(lib_xbrick(1512),key=1512, & 
& nodecnc=[162,1295,1271,1300,2734,3867,3843,3872,20581, 20582,20492, 20491,10934, 10933,20583, 20584 & 
& ,20585, 20586,20496, 20495,10940, 10939,20587, 20588], & 
& edgecnc=[7719,7674,2895,7720,7721,7676,2898,7722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1512),elname="xbrick",eltype="xbrick",typekey=1512) 

        call prepare(lib_xbrick(1513),key=1513, & 
& nodecnc=[1287,343,1179,1282,3859,2915,3751,3854,20589, 20590,19712, 19711,20368, 20367,13800, 13799 & 
& ,20591, 20592,19716, 19715,20372, 20371,13804, 13803], & 
& edgecnc=[7723,7284,7612,4328,7724,7286,7614,4330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1513),elname="xbrick",eltype="xbrick",typekey=1513) 

        call prepare(lib_xbrick(1514),key=1514, & 
& nodecnc=[1279,2254,1284,1272,3851,4826,3856,3844,20593, 20594,9852, 9851,9868, 9867,20595, 20596,20597 & 
& , 20598,9860, 9859,9876, 9875,20599, 20600], & 
& edgecnc=[7725,2354,2362,7726,7727,2358,2366,7728], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1514),elname="xbrick",eltype="xbrick",typekey=1514) 

        call prepare(lib_xbrick(1515),key=1515, & 
& nodecnc=[1290,1292,2440,1274,3862,3864,5012,3846,20601, 20602,7172, 7171,20603, 20604,10870, 10869,20605 & 
& , 20606,7180, 7179,20607, 20608,10878, 10877], & 
& edgecnc=[7729,1014,7730,2863,7731,1018,7732,2867], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1515),elname="xbrick",eltype="xbrick",typekey=1515) 

        call prepare(lib_xbrick(1516),key=1516, & 
& nodecnc=[2390,1288,1269,1297,4962,3860,3841,3869,19436, 19435,19466, 19465,10962, 10961,12096, 12095 & 
& ,19444, 19443,19472, 19471,10970, 10969,12104, 12103], & 
& edgecnc=[7146,7161,2909,3476,7150,7164,2913,3480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1516),elname="xbrick",eltype="xbrick",typekey=1516) 

        call prepare(lib_xbrick(1517),key=1517, & 
& nodecnc=[1275,1276,2337,1316,3847,3848,4909,3888,20550, 20549,20609, 20610,20611, 20612,20613, 20614 & 
& ,20554, 20553,20615, 20616,20617, 20618,20619, 20620], & 
& edgecnc=[7703,7733,7734,7735,7705,7736,7737,7738], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1517),elname="xbrick",eltype="xbrick",typekey=1517) 

        call prepare(lib_xbrick(1518),key=1518, & 
& nodecnc=[178,1291,1276,1270,2750,3863,3848,3842,20621, 20622,20623, 20624,20552, 20551,20560, 20559 & 
& ,20625, 20626,20627, 20628,20556, 20555,20564, 20563], & 
& edgecnc=[7739,7740,7704,7708,7741,7742,7706,7710], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1518),elname="xbrick",eltype="xbrick",typekey=1518) 

        call prepare(lib_xbrick(1519),key=1519, & 
& nodecnc=[1298,2337,1276,1291,3870,4909,3848,3863,20629, 20630,20610, 20609,20624, 20623,20631, 20632 & 
& ,20633, 20634,20616, 20615,20628, 20627,20635, 20636], & 
& edgecnc=[7743,7733,7740,7744,7745,7736,7742,7746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1519),elname="xbrick",eltype="xbrick",typekey=1519) 

        call prepare(lib_xbrick(1520),key=1520, & 
& nodecnc=[2417,1277,1288,1289,4989,3849,3860,3861,19596, 19595,19462, 19461,19434, 19433,20637, 20638 & 
& ,19602, 19601,19468, 19467,19442, 19441,20639, 20640], & 
& edgecnc=[7226,7159,7145,7747,7229,7162,7149,7748], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1520),elname="xbrick",eltype="xbrick",typekey=1520) 

        call prepare(lib_xbrick(1521),key=1521, & 
& nodecnc=[1266,1294,1296,1278,3838,3866,3868,3850,20504, 20503,11358, 11357,5648, 5647,12790, 12789,20508 & 
& , 20507,11364, 11363,5656, 5655,12796, 12795], & 
& edgecnc=[7680,3107,252,3823,7682,3110,256,3826], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1521),elname="xbrick",eltype="xbrick",typekey=1521) 

        call prepare(lib_xbrick(1522),key=1522, & 
& nodecnc=[2253,1279,1272,313,4825,3851,3844,2885,20641, 20642,20596, 20595,12794, 12793,20643, 20644 & 
& ,20645, 20646,20600, 20599,12800, 12799,20647, 20648], & 
& edgecnc=[7749,7726,3825,7750,7751,7728,3828,7752], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1522),elname="xbrick",eltype="xbrick",typekey=1522) 

        call prepare(lib_xbrick(1523),key=1523, & 
& nodecnc=[344,1262,1281,1282,2916,3834,3853,3854,20534, 20533,13806, 13805,13798, 13797,20366, 20365 & 
& ,20536, 20535,13810, 13809,13802, 13801,20370, 20369], & 
& edgecnc=[7695,4331,4327,7611,7696,4333,4329,7613], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1523),elname="xbrick",eltype="xbrick",typekey=1523) 

        call prepare(lib_xbrick(1524),key=1524, & 
& nodecnc=[2232,1633,1286,2254,4804,4205,3858,4826,20649, 20650,20651, 20652,9854, 9853,20653, 20654,20655 & 
& , 20656,20657, 20658,9862, 9861,20659, 20660], & 
& edgecnc=[7753,7754,2355,7755,7756,7757,2359,7758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1524),elname="xbrick",eltype="xbrick",typekey=1524) 

        call prepare(lib_xbrick(1525),key=1525, & 
& nodecnc=[1633,346,1267,1286,4205,2918,3839,3858,20661, 20662,20430, 20429,20530, 20529,20652, 20651 & 
& ,20663, 20664,20436, 20435,20532, 20531,20658, 20657], & 
& edgecnc=[7759,7643,7693,7754,7760,7646,7694,7757], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1525),elname="xbrick",eltype="xbrick",typekey=1525) 

        call prepare(lib_xbrick(1526),key=1526, & 
& nodecnc=[1289,211,1304,2417,3861,2783,3876,4989,19432, 19431,20665, 20666,20667, 20668,20638, 20637 & 
& ,19440, 19439,20669, 20670,20671, 20672,20640, 20639], & 
& edgecnc=[7144,7761,7762,7747,7148,7763,7764,7748], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1526),elname="xbrick",eltype="xbrick",typekey=1526) 

        call prepare(lib_xbrick(1527),key=1527, & 
& nodecnc=[1290,303,1632,1641,3862,2875,4204,4213,20518, 20517,20673, 20674,10912, 10911,20675, 20676 & 
& ,20520, 20519,20677, 20678,10918, 10917,20679, 20680], & 
& edgecnc=[7687,7765,2884,7766,7688,7767,2887,7768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1527),elname="xbrick",eltype="xbrick",typekey=1527) 

        call prepare(lib_xbrick(1528),key=1528, & 
& nodecnc=[1320,2446,1307,1329,3892,5018,3879,3901,11558, 11557,20681, 20682,20683, 20684,11502, 11501 & 
& ,11566, 11565,20685, 20686,20687, 20688,11510, 11509], & 
& edgecnc=[3207,7769,7770,3179,3211,7771,7772,3183], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1528),elname="xbrick",eltype="xbrick",typekey=1528) 

        call prepare(lib_xbrick(1529),key=1529, & 
& nodecnc=[302,1638,1292,2377,2874,4210,3864,4949,12696, 12695,7174, 7173,20689, 20690,20691, 20692,12704 & 
& , 12703,7182, 7181,20693, 20694,20695, 20696], & 
& edgecnc=[3776,1015,7773,7774,3780,1019,7775,7776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1529),elname="xbrick",eltype="xbrick",typekey=1529) 

        call prepare(lib_xbrick(1530),key=1530, & 
& nodecnc=[2500,2440,1315,1301,5072,5012,3887,3873,20697, 20698,7170, 7169,10888, 10887,10798, 10797,20699 & 
& , 20700,7178, 7177,10896, 10895,10806, 10805], & 
& edgecnc=[7777,1013,2872,2827,7778,1017,2876,2831], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1530),elname="xbrick",eltype="xbrick",typekey=1530) 

        call prepare(lib_xbrick(1531),key=1531, & 
& nodecnc=[1326,314,1293,1303,3898,2886,3865,3875,20701, 20702,20703, 20704,20574, 20573,5644, 5643,20705 & 
& , 20706,20707, 20708,20578, 20577,5652, 5651], & 
& edgecnc=[7779,7780,7715,250,7781,7782,7717,254], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1531),elname="xbrick",eltype="xbrick",typekey=1531) 

        call prepare(lib_xbrick(1532),key=1532, & 
& nodecnc=[1304,211,2322,1305,3876,2783,4894,3877,20666, 20665,20709, 20710,10984, 10983,20711, 20712 & 
& ,20670, 20669,20713, 20714,10992, 10991,20715, 20716], & 
& edgecnc=[7761,7783,2920,7784,7763,7785,2924,7786], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1532),elname="xbrick",eltype="xbrick",typekey=1532) 

        call prepare(lib_xbrick(1533),key=1533, & 
& nodecnc=[1275,1316,1318,1297,3847,3888,3890,3869,20614, 20613,20717, 20718,12090, 12089,10968, 10967 & 
& ,20620, 20619,20719, 20720,12098, 12097,10976, 10975], & 
& edgecnc=[7735,7787,3473,2912,7738,7788,3477,2916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1533),elname="xbrick",eltype="xbrick",typekey=1533) 

        call prepare(lib_xbrick(1534),key=1534, & 
& nodecnc=[1335,1371,1374,1375,3907,3943,3946,3947,12116, 12115,20721, 20722,20723, 20724,20725, 20726 & 
& ,12122, 12121,20727, 20728,20729, 20730,20731, 20732], & 
& edgecnc=[3486,7789,7790,7791,3489,7792,7793,7794], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1534),elname="xbrick",eltype="xbrick",typekey=1534) 

        call prepare(lib_xbrick(1535),key=1535, & 
& nodecnc=[2391,1321,1298,1311,4963,3893,3870,3883,20733, 20734,12114, 12113,20735, 20736,20737, 20738 & 
& ,20739, 20740,12120, 12119,20741, 20742,20743, 20744], & 
& edgecnc=[7795,3485,7796,7797,7798,3488,7799,7800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1535),elname="xbrick",eltype="xbrick",typekey=1535) 

        call prepare(lib_xbrick(1536),key=1536, & 
& nodecnc=[1307,2446,178,1264,3879,5018,2750,3836,20682, 20681,20745, 20746,20558, 20557,11954, 11953 & 
& ,20686, 20685,20747, 20748,20562, 20561,11962, 11961], & 
& edgecnc=[7769,7801,7707,3405,7771,7802,7709,3409], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1536),elname="xbrick",eltype="xbrick",typekey=1536) 

        call prepare(lib_xbrick(1537),key=1537, & 
& nodecnc=[1328,162,1300,1314,3900,2734,3872,3886,20749, 20750,20584, 20583,20751, 20752,11492, 11491 & 
& ,20753, 20754,20588, 20587,20755, 20756,11498, 11497], & 
& edgecnc=[7803,7720,7804,3174,7805,7722,7806,3177], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1537),elname="xbrick",eltype="xbrick",typekey=1537) 

        call prepare(lib_xbrick(1538),key=1538, & 
& nodecnc=[2500,1314,1300,133,5072,3886,3872,2705,10796, 10795,20752, 20751,10938, 10937,20757, 20758 & 
& ,10804, 10803,20756, 20755,10944, 10943,20759, 20760], & 
& edgecnc=[2826,7804,2897,7807,2830,7806,2900,7808], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1538),elname="xbrick",eltype="xbrick",typekey=1538) 

        call prepare(lib_xbrick(1539),key=1539, & 
& nodecnc=[2519,1858,132,1884,5091,4430,2704,4456,20761, 20762,10860, 10859,10884, 10883,20763, 20764 & 
& ,20765, 20766,10864, 10863,10892, 10891,20767, 20768], & 
& edgecnc=[7809,2858,2870,7810,7811,2860,2874,7812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1539),elname="xbrick",eltype="xbrick",typekey=1539) 

        call prepare(lib_xbrick(1540),key=1540, & 
& nodecnc=[146,1312,314,1326,2718,3884,2886,3898,20769, 20770,12804, 12803,20702, 20701,20771, 20772,20773 & 
& , 20774,12812, 12811,20706, 20705,20775, 20776], & 
& edgecnc=[7813,3830,7779,7814,7815,3834,7781,7816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1540),elname="xbrick",eltype="xbrick",typekey=1540) 

        call prepare(lib_xbrick(1541),key=1541, & 
& nodecnc=[1333,1306,1304,1305,3905,3878,3876,3877,20777, 20778,20779, 20780,20712, 20711,20781, 20782 & 
& ,20783, 20784,20785, 20786,20716, 20715,20787, 20788], & 
& edgecnc=[7817,7818,7784,7819,7820,7821,7786,7822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1541),elname="xbrick",eltype="xbrick",typekey=1541) 

        call prepare(lib_xbrick(1542),key=1542, & 
& nodecnc=[1334,1317,2322,1308,3906,3889,4894,3880,20789, 20790,10978, 10977,20791, 20792,20793, 20794 & 
& ,20795, 20796,10986, 10985,20797, 20798,20799, 20800], & 
& edgecnc=[7823,2917,7824,7825,7826,2921,7827,7828], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1542),elname="xbrick",eltype="xbrick",typekey=1542) 

        call prepare(lib_xbrick(1543),key=1543, & 
& nodecnc=[2417,1304,1306,1123,4989,3876,3878,3695,20668, 20667,20780, 20779,11062, 11061,19694, 19693 & 
& ,20672, 20671,20786, 20785,11070, 11069,19696, 19695], & 
& edgecnc=[7762,7818,2959,7275,7764,7821,2963,7276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1543),elname="xbrick",eltype="xbrick",typekey=1543) 

        call prepare(lib_xbrick(1544),key=1544, & 
& nodecnc=[1306,1333,1319,2362,3878,3905,3891,4934,20778, 20777,19674, 19673,19726, 19725,11064, 11063 & 
& ,20784, 20783,19682, 19681,19732, 19731,11072, 11071], & 
& edgecnc=[7817,7265,7291,2960,7820,7269,7294,2964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1544),elname="xbrick",eltype="xbrick",typekey=1544) 

        call prepare(lib_xbrick(1545),key=1545, & 
& nodecnc=[2445,1291,178,2446,5017,3863,2750,5018,20801, 20802,20622, 20621,20746, 20745,11564, 11563 & 
& ,20803, 20804,20626, 20625,20748, 20747,11572, 11571], & 
& edgecnc=[7829,7739,7801,3210,7830,7741,7802,3214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1545),elname="xbrick",eltype="xbrick",typekey=1545) 

        call prepare(lib_xbrick(1546),key=1546, & 
& nodecnc=[1330,1334,1308,1318,3902,3906,3880,3890,20805, 20806,20794, 20793,12092, 12091,20807, 20808 & 
& ,20809, 20810,20800, 20799,12100, 12099,20811, 20812], & 
& edgecnc=[7831,7825,3474,7832,7833,7828,3478,7834], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1546),elname="xbrick",eltype="xbrick",typekey=1546) 

        call prepare(lib_xbrick(1547),key=1547, & 
& nodecnc=[342,2236,1309,2229,2914,4808,3881,4801,19296, 19295,19750, 19749,20813, 20814,19870, 19869 & 
& ,19302, 19301,19754, 19753,20815, 20816,19874, 19873], & 
& edgecnc=[7076,7303,7835,7363,7079,7305,7836,7365], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1547),elname="xbrick",eltype="xbrick",typekey=1547) 

        call prepare(lib_xbrick(1548),key=1548, & 
& nodecnc=[2229,1309,1323,1159,4801,3881,3895,3731,20814, 20813,13786, 13785,19858, 19857,19872, 19871 & 
& ,20816, 20815,13792, 13791,19864, 19863,19876, 19875], & 
& edgecnc=[7835,4321,7357,7364,7836,4324,7360,7366], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1548),elname="xbrick",eltype="xbrick",typekey=1548) 

        call prepare(lib_xbrick(1549),key=1549, & 
& nodecnc=[1337,1338,1324,1310,3909,3910,3896,3882,20817, 20818,20819, 20820,5658, 5657,20821, 20822,20823 & 
& , 20824,20825, 20826,5664, 5663,20827, 20828], & 
& edgecnc=[7837,7838,257,7839,7840,7841,260,7842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1549),elname="xbrick",eltype="xbrick",typekey=1549) 

        call prepare(lib_xbrick(1550),key=1550, & 
& nodecnc=[145,1325,1337,1310,2717,3897,3909,3882,20354, 20353,20829, 20830,20822, 20821,11356, 11355 & 
& ,20360, 20359,20831, 20832,20828, 20827,11362, 11361], & 
& edgecnc=[7605,7843,7839,3106,7608,7844,7842,3109], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1550),elname="xbrick",eltype="xbrick",typekey=1550) 

        call prepare(lib_xbrick(1551),key=1551, & 
& nodecnc=[213,1321,2391,1336,2785,3893,4963,3908,11574, 11573,20734, 20733,20833, 20834,12108, 12107 & 
& ,11582, 11581,20740, 20739,20835, 20836,12112, 12111], & 
& edgecnc=[3215,7795,7845,3482,3219,7798,7846,3484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1551),elname="xbrick",eltype="xbrick",typekey=1551) 

        call prepare(lib_xbrick(1552),key=1552, & 
& nodecnc=[2391,1311,1320,179,4963,3883,3892,2751,20738, 20737,11560, 11559,11508, 11507,20837, 20838 & 
& ,20744, 20743,11568, 11567,11516, 11515,20839, 20840], & 
& edgecnc=[7797,3208,3182,7847,7800,3212,3186,7848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1552),elname="xbrick",eltype="xbrick",typekey=1552) 

        call prepare(lib_xbrick(1553),key=1553, & 
& nodecnc=[1431,315,1428,1327,4003,2887,4000,3899,9498, 9497,20841, 20842,8894, 8893,20843, 20844,9506 & 
& , 9505,20845, 20846,8902, 8901,20847, 20848], & 
& edgecnc=[2177,7849,1875,7850,2181,7851,1879,7852], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1553),elname="xbrick",eltype="xbrick",typekey=1553) 

        call prepare(lib_xbrick(1554),key=1554, & 
& nodecnc=[1323,1313,463,72,3895,3885,3035,2644,13790, 13789,11284, 11283,20849, 20850,19860, 19859,13796 & 
& , 13795,11292, 11291,20851, 20852,19866, 19865], & 
& edgecnc=[4323,3070,7853,7358,4326,3074,7854,7361], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1554),elname="xbrick",eltype="xbrick",typekey=1554) 

        call prepare(lib_xbrick(1555),key=1555, & 
& nodecnc=[2502,2524,161,1226,5074,5096,2733,3798,20853, 20854,11950, 11949,20256, 20255,20332, 20331 & 
& ,20855, 20856,11958, 11957,20260, 20259,20336, 20335], & 
& edgecnc=[7855,3403,7556,7594,7856,3407,7558,7596], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1555),elname="xbrick",eltype="xbrick",typekey=1555) 

        call prepare(lib_xbrick(1556),key=1556, & 
& nodecnc=[1638,1937,1884,1315,4210,4509,4456,3887,12694, 12693,20857, 20858,10882, 10881,7176, 7175,12702 & 
& , 12701,20859, 20860,10890, 10889,7184, 7183], & 
& edgecnc=[3775,7857,2869,1016,3779,7858,2873,1020], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1556),elname="xbrick",eltype="xbrick",typekey=1556) 

        call prepare(lib_xbrick(1557),key=1557, & 
& nodecnc=[2304,1317,1346,271,4876,3889,3918,2843,10980, 10979,20861, 20862,20863, 20864,20865, 20866 & 
& ,10988, 10987,20867, 20868,20869, 20870,20871, 20872], & 
& edgecnc=[2918,7859,7860,7861,2922,7862,7863,7864], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1557),elname="xbrick",eltype="xbrick",typekey=1557) 

        call prepare(lib_xbrick(1558),key=1558, & 
& nodecnc=[1330,1387,270,1334,3902,3959,2842,3906,20873, 20874,20875, 20876,20877, 20878,20806, 20805 & 
& ,20879, 20880,20881, 20882,20883, 20884,20810, 20809], & 
& edgecnc=[7865,7866,7867,7831,7868,7869,7870,7833], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1558),elname="xbrick",eltype="xbrick",typekey=1558) 

        call prepare(lib_xbrick(1559),key=1559, & 
& nodecnc=[1330,1318,1316,212,3902,3890,3888,2784,20808, 20807,20718, 20717,20885, 20886,20887, 20888 & 
& ,20812, 20811,20720, 20719,20889, 20890,20891, 20892], & 
& edgecnc=[7832,7787,7871,7872,7834,7788,7873,7874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1559),elname="xbrick",eltype="xbrick",typekey=1559) 

        call prepare(lib_xbrick(1560),key=1560, & 
& nodecnc=[1291,2445,1311,1298,3863,5017,3883,3870,20802, 20801,11562, 11561,20736, 20735,20632, 20631 & 
& ,20804, 20803,11570, 11569,20742, 20741,20636, 20635], & 
& edgecnc=[7829,3209,7796,7744,7830,3213,7799,7746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1560),elname="xbrick",eltype="xbrick",typekey=1560) 

        call prepare(lib_xbrick(1561),key=1561, & 
& nodecnc=[2337,1298,1335,2302,4909,3870,3907,4874,20630, 20629,12118, 12117,20893, 20894,20895, 20896 & 
& ,20634, 20633,12124, 12123,20897, 20898,20899, 20900], & 
& edgecnc=[7743,3487,7875,7876,7745,3490,7877,7878], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1561),elname="xbrick",eltype="xbrick",typekey=1561) 

        call prepare(lib_xbrick(1562),key=1562, & 
& nodecnc=[1326,1324,1331,146,3898,3896,3903,2718,5660, 5659,20901, 20902,20903, 20904,20772, 20771,5666 & 
& , 5665,20905, 20906,20907, 20908,20776, 20775], & 
& edgecnc=[258,7879,7880,7814,261,7881,7882,7816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1562),elname="xbrick",eltype="xbrick",typekey=1562) 

        call prepare(lib_xbrick(1563),key=1563, & 
& nodecnc=[1359,99,1642,1367,3931,2671,4214,3939,20909, 20910,9978, 9977,20911, 20912,20172, 20171,20913 & 
& , 20914,9986, 9985,20915, 20916,20178, 20177], & 
& edgecnc=[7883,2417,7884,7514,7885,2421,7886,7517], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1563),elname="xbrick",eltype="xbrick",typekey=1563) 

        call prepare(lib_xbrick(1564),key=1564, & 
& nodecnc=[2359,147,1395,1431,4931,2719,3967,4003,20917, 20918,9482, 9481,9500, 9499,20919, 20920,20921 & 
& , 20922,9490, 9489,9508, 9507,20923, 20924], & 
& edgecnc=[7887,2169,2178,7888,7889,2173,2182,7890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1564),elname="xbrick",eltype="xbrick",typekey=1564) 

        call prepare(lib_xbrick(1565),key=1565, & 
& nodecnc=[1295,162,1328,1624,3867,2734,3900,4196,20582, 20581,20750, 20749,20925, 20926,10946, 10945 & 
& ,20586, 20585,20754, 20753,20927, 20928,10954, 10953], & 
& edgecnc=[7719,7803,7891,2901,7721,7805,7892,2905], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1565),elname="xbrick",eltype="xbrick",typekey=1565) 

        call prepare(lib_xbrick(1566),key=1566, & 
& nodecnc=[1329,1624,1340,1345,3901,4196,3912,3917,20929, 20930,20931, 20932,20933, 20934,11504, 11503 & 
& ,20935, 20936,20937, 20938,20939, 20940,11512, 11511], & 
& edgecnc=[7893,7894,7895,3180,7896,7897,7898,3184], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1566),elname="xbrick",eltype="xbrick",typekey=1566) 

        call prepare(lib_xbrick(1567),key=1567, & 
& nodecnc=[269,1374,1371,1361,2841,3946,3943,3933,20941, 20942,20722, 20721,11578, 11577,20943, 20944 & 
& ,20945, 20946,20728, 20727,11586, 11585,20947, 20948], & 
& edgecnc=[7899,7789,3217,7900,7901,7792,3221,7902], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1567),elname="xbrick",eltype="xbrick",typekey=1567) 

        call prepare(lib_xbrick(1568),key=1568, & 
& nodecnc=[1353,1362,2450,1348,3925,3934,5022,3920,20949, 20950,20951, 20952,20953, 20954,11366, 11365 & 
& ,20955, 20956,20957, 20958,20959, 20960,11372, 11371], & 
& edgecnc=[7903,7904,7905,3111,7906,7907,7908,3114], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1568),elname="xbrick",eltype="xbrick",typekey=1568) 

        call prepare(lib_xbrick(1569),key=1569, & 
& nodecnc=[2506,1331,1324,1338,5078,3903,3896,3910,20961, 20962,20902, 20901,20820, 20819,20963, 20964 & 
& ,20965, 20966,20906, 20905,20826, 20825,20967, 20968], & 
& edgecnc=[7909,7879,7838,7910,7911,7881,7841,7912], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1569),elname="xbrick",eltype="xbrick",typekey=1569) 

        call prepare(lib_xbrick(1570),key=1570, & 
& nodecnc=[1634,1333,1305,2304,4206,3905,3877,4876,19676, 19675,20782, 20781,10982, 10981,20969, 20970 & 
& ,19684, 19683,20788, 20787,10990, 10989,20971, 20972], & 
& edgecnc=[7266,7819,2919,7913,7270,7822,2923,7914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1570),elname="xbrick",eltype="xbrick",typekey=1570) 

        call prepare(lib_xbrick(1571),key=1571, & 
& nodecnc=[270,1346,1317,1334,2842,3918,3889,3906,20973, 20974,20862, 20861,20790, 20789,20878, 20877 & 
& ,20975, 20976,20868, 20867,20796, 20795,20884, 20883], & 
& edgecnc=[7915,7859,7823,7867,7916,7862,7826,7870], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1571),elname="xbrick",eltype="xbrick",typekey=1571) 

        call prepare(lib_xbrick(1572),key=1572, & 
& nodecnc=[1387,1330,212,1375,3959,3902,2784,3947,20874, 20873,20888, 20887,20977, 20978,20979, 20980 & 
& ,20880, 20879,20892, 20891,20981, 20982,20983, 20984], & 
& edgecnc=[7865,7872,7917,7918,7868,7874,7919,7920], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1572),elname="xbrick",eltype="xbrick",typekey=1572) 

        call prepare(lib_xbrick(1573),key=1573, & 
& nodecnc=[179,1351,1336,2391,2751,3923,3908,4963,20985, 20986,11684, 11683,20834, 20833,20838, 20837 & 
& ,20987, 20988,11692, 11691,20836, 20835,20840, 20839], & 
& edgecnc=[7921,3270,7845,7847,7922,3274,7846,7848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1573),elname="xbrick",eltype="xbrick",typekey=1573) 

        call prepare(lib_xbrick(1574),key=1574, & 
& nodecnc=[1358,2545,2546,1343,3930,5117,5118,3915,20989, 20990,20991, 20992,20993, 20994,9974, 9973,20995 & 
& , 20996,20997, 20998,20999, 21000,9982, 9981], & 
& edgecnc=[7923,7924,7925,2415,7926,7927,7928,2419], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1574),elname="xbrick",eltype="xbrick",typekey=1574) 

        call prepare(lib_xbrick(1575),key=1575, & 
& nodecnc=[1325,1642,1343,1337,3897,4214,3915,3909,21001, 21002,9976, 9975,21003, 21004,20830, 20829,21005 & 
& , 21006,9984, 9983,21007, 21008,20832, 20831], & 
& edgecnc=[7929,2416,7930,7843,7931,2420,7932,7844], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1575),elname="xbrick",eltype="xbrick",typekey=1575) 

        call prepare(lib_xbrick(1576),key=1576, & 
& nodecnc=[1338,1337,1343,2546,3910,3909,3915,5118,20818, 20817,21004, 21003,20994, 20993,21009, 21010 & 
& ,20824, 20823,21008, 21007,21000, 20999,21011, 21012], & 
& edgecnc=[7837,7930,7925,7933,7840,7932,7928,7934], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1576),elname="xbrick",eltype="xbrick",typekey=1576) 

        call prepare(lib_xbrick(1577),key=1577, & 
& nodecnc=[1647,1339,1709,2550,4219,3911,4281,5122,11494, 11493,21013, 21014,21015, 21016,21017, 21018 & 
& ,11500, 11499,21019, 21020,21021, 21022,21023, 21024], & 
& edgecnc=[3175,7935,7936,7937,3178,7938,7939,7940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1577),elname="xbrick",eltype="xbrick",typekey=1577) 

        call prepare(lib_xbrick(1578),key=1578, & 
& nodecnc=[1709,1339,1301,132,4281,3911,3873,2704,21014, 21013,10800, 10799,10886, 10885,10858, 10857 & 
& ,21020, 21019,10808, 10807,10894, 10893,10862, 10861], & 
& edgecnc=[7935,2828,2871,2857,7938,2832,2875,2859], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1578),elname="xbrick",eltype="xbrick",typekey=1578) 

        call prepare(lib_xbrick(1579),key=1579, & 
& nodecnc=[2492,1378,1340,2530,5064,3950,3912,5102,21025, 21026,21027, 21028,21029, 21030,11548, 11547 & 
& ,21031, 21032,21033, 21034,21035, 21036,11554, 11553], & 
& edgecnc=[7941,7942,7943,3202,7944,7945,7946,3205], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1579),elname="xbrick",eltype="xbrick",typekey=1579) 

        call prepare(lib_xbrick(1580),key=1580, & 
& nodecnc=[1647,1340,1624,1328,4219,3912,4196,3900,21037, 21038,20932, 20931,20926, 20925,11490, 11489 & 
& ,21039, 21040,20938, 20937,20928, 20927,11496, 11495], & 
& edgecnc=[7947,7894,7891,3173,7948,7897,7892,3176], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1580),elname="xbrick",eltype="xbrick",typekey=1580) 

        call prepare(lib_xbrick(1581),key=1581, & 
& nodecnc=[1362,147,2359,2450,3934,2719,4931,5022,21041, 21042,20918, 20917,21043, 21044,20952, 20951 & 
& ,21045, 21046,20922, 20921,21047, 21048,20958, 20957], & 
& edgecnc=[7949,7887,7950,7904,7951,7889,7952,7907], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1581),elname="xbrick",eltype="xbrick",typekey=1581) 

        call prepare(lib_xbrick(1582),key=1582, & 
& nodecnc=[1363,1354,1349,1342,3935,3926,3921,3914,13070, 13069,21049, 21050,12808, 12807,8898, 8897,13078 & 
& , 13077,21051, 21052,12816, 12815,8906, 8905], & 
& edgecnc=[3963,7953,3832,1877,3967,7954,3836,1881], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1582),elname="xbrick",eltype="xbrick",typekey=1582) 

        call prepare(lib_xbrick(1583),key=1583, & 
& nodecnc=[2174,1630,100,2568,4746,4202,2672,5140,21053, 21054,9992, 9991,21055, 21056,21057, 21058,21059 & 
& , 21060,9998, 9997,21061, 21062,21063, 21064], & 
& edgecnc=[7955,2424,7956,7957,7958,2427,7959,7960], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1583),elname="xbrick",eltype="xbrick",typekey=1583) 

        call prepare(lib_xbrick(1584),key=1584, & 
& nodecnc=[1936,2566,1358,99,4508,5138,3930,2671,21065, 21066,21067, 21068,9980, 9979,21069, 21070,21071 & 
& , 21072,21073, 21074,9988, 9987,21075, 21076], & 
& edgecnc=[7961,7962,2418,7963,7964,7965,2422,7966], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1584),elname="xbrick",eltype="xbrick",typekey=1584) 

        call prepare(lib_xbrick(1585),key=1585, & 
& nodecnc=[1181,2231,2213,237,3753,4803,4785,2809,21077, 21078,21079, 21080,21081, 21082,20142, 20141 & 
& ,21083, 21084,21085, 21086,21087, 21088,20144, 20143], & 
& edgecnc=[7967,7968,7969,7499,7970,7971,7972,7500], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1585),elname="xbrick",eltype="xbrick",typekey=1585) 

        call prepare(lib_xbrick(1586),key=1586, & 
& nodecnc=[1332,1634,1350,1356,3904,4206,3922,3928,19678, 19677,21089, 21090,21091, 21092,11042, 11041 & 
& ,19686, 19685,21093, 21094,21095, 21096,11050, 11049], & 
& edgecnc=[7267,7973,7974,2949,7271,7975,7976,2953], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1586),elname="xbrick",eltype="xbrick",typekey=1586) 

        call prepare(lib_xbrick(1587),key=1587, & 
& nodecnc=[1345,2468,1351,179,3917,5040,3923,2751,21097, 21098,21099, 21100,20986, 20985,11506, 11505 & 
& ,21101, 21102,21103, 21104,20988, 20987,11514, 11513], & 
& edgecnc=[7977,7978,7921,3181,7979,7980,7922,3185], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1587),elname="xbrick",eltype="xbrick",typekey=1587) 

        call prepare(lib_xbrick(1588),key=1588, & 
& nodecnc=[1378,2492,1357,2468,3950,5064,3929,5040,21026, 21025,21105, 21106,21107, 21108,21109, 21110 & 
& ,21032, 21031,21111, 21112,21113, 21114,21115, 21116], & 
& edgecnc=[7941,7981,7982,7983,7944,7984,7985,7986], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1588),elname="xbrick",eltype="xbrick",typekey=1588) 

        call prepare(lib_xbrick(1589),key=1589, & 
& nodecnc=[1372,1361,213,1352,3944,3933,2785,3924,21117, 21118,11576, 11575,12106, 12105,21119, 21120 & 
& ,21121, 21122,11584, 11583,12110, 12109,21123, 21124], & 
& edgecnc=[7987,3216,3481,7988,7989,3220,3483,7990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1589),elname="xbrick",eltype="xbrick",typekey=1589) 

        call prepare(lib_xbrick(1590),key=1590, & 
& nodecnc=[1312,146,1348,1341,3884,2718,3920,3913,20770, 20769,21125, 21126,21127, 21128,12984, 12983 & 
& ,20774, 20773,21129, 21130,21131, 21132,12988, 12987], & 
& edgecnc=[7813,7991,7992,3920,7815,7993,7994,3922], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1590),elname="xbrick",eltype="xbrick",typekey=1590) 

        call prepare(lib_xbrick(1591),key=1591, & 
& nodecnc=[1619,1349,1354,348,4191,3921,3926,2920,21133, 21134,21050, 21049,21135, 21136,5632, 5631,21137 & 
& , 21138,21052, 21051,21139, 21140,5640, 5639], & 
& edgecnc=[7995,7953,7996,244,7997,7954,7998,248], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1591),elname="xbrick",eltype="xbrick",typekey=1591) 

        call prepare(lib_xbrick(1592),key=1592, & 
& nodecnc=[1619,1293,314,1349,4191,3865,2886,3921,21141, 21142,20704, 20703,12802, 12801,21134, 21133 & 
& ,21143, 21144,20708, 20707,12810, 12809,21138, 21137], & 
& edgecnc=[7999,7780,3829,7995,8000,7782,3833,7997], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1592),elname="xbrick",eltype="xbrick",typekey=1592) 

        call prepare(lib_xbrick(1593),key=1593, & 
& nodecnc=[236,1366,1350,1376,2808,3938,3922,3948,13346, 13345,21145, 21146,21147, 21148,21149, 21150 & 
& ,13350, 13349,21151, 21152,21153, 21154,21155, 21156], & 
& edgecnc=[4101,8001,8002,8003,4103,8004,8005,8006], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1593),elname="xbrick",eltype="xbrick",typekey=1593) 

        call prepare(lib_xbrick(1594),key=1594, & 
& nodecnc=[1376,1350,1634,2289,3948,3922,4206,4861,21148, 21147,21090, 21089,21157, 21158,21159, 21160 & 
& ,21154, 21153,21094, 21093,21161, 21162,21163, 21164], & 
& edgecnc=[8002,7973,8007,8008,8005,7975,8009,8010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1594),elname="xbrick",eltype="xbrick",typekey=1594) 

        call prepare(lib_xbrick(1595),key=1595, & 
& nodecnc=[2468,1357,1360,1351,5040,3929,3932,3923,21108, 21107,21165, 21166,11686, 11685,21100, 21099 & 
& ,21114, 21113,21167, 21168,11694, 11693,21104, 21103], & 
& edgecnc=[7982,8011,3271,7978,7985,8012,3275,7980], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1595),elname="xbrick",eltype="xbrick",typekey=1595) 

        call prepare(lib_xbrick(1596),key=1596, & 
& nodecnc=[1403,1383,1372,1352,3975,3955,3944,3924,21169, 21170,21171, 21172,21120, 21119,11670, 11669 & 
& ,21173, 21174,21175, 21176,21124, 21123,11678, 11677], & 
& edgecnc=[8013,8014,7988,3263,8015,8016,7990,3267], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1596),elname="xbrick",eltype="xbrick",typekey=1596) 

        call prepare(lib_xbrick(1597),key=1597, & 
& nodecnc=[1369,348,1354,1355,3941,2920,3926,3927,13850, 13849,21136, 21135,13076, 13075,8884, 8883,13856 & 
& , 13855,21140, 21139,13084, 13083,8892, 8891], & 
& edgecnc=[4353,7996,3966,1870,4356,7998,3970,1874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1597),elname="xbrick",eltype="xbrick",typekey=1597) 

        call prepare(lib_xbrick(1598),key=1598, & 
& nodecnc=[2231,1356,1370,2213,4803,3928,3942,4785,11044, 11043,21177, 21178,21179, 21180,21080, 21079 & 
& ,11052, 11051,21181, 21182,21183, 21184,21086, 21085], & 
& edgecnc=[2950,8017,8018,7968,2954,8019,8020,7971], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1598),elname="xbrick",eltype="xbrick",typekey=1598) 

        call prepare(lib_xbrick(1599),key=1599, & 
& nodecnc=[1350,1366,1370,1356,3922,3938,3942,3928,21146, 21145,11032, 11031,21178, 21177,21092, 21091 & 
& ,21152, 21151,11040, 11039,21182, 21181,21096, 21095], & 
& edgecnc=[8001,2944,8017,7974,8004,2948,8019,7976], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1599),elname="xbrick",eltype="xbrick",typekey=1599) 

        call prepare(lib_xbrick(1600),key=1600, & 
& nodecnc=[2492,1365,180,1357,5064,3937,2752,3929,11546, 11545,11522, 11521,21185, 21186,21106, 21105 & 
& ,11552, 11551,11528, 11527,21187, 21188,21112, 21111], & 
& edgecnc=[3201,3189,8021,7981,3204,3192,8022,7984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1600),elname="xbrick",eltype="xbrick",typekey=1600) 

        call prepare(lib_xbrick(1601),key=1601, & 
& nodecnc=[1340,1378,2468,1345,3912,3950,5040,3917,21028, 21027,21110, 21109,21098, 21097,20934, 20933 & 
& ,21034, 21033,21116, 21115,21102, 21101,20940, 20939], & 
& edgecnc=[7942,7983,7977,7895,7945,7986,7979,7898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1601),elname="xbrick",eltype="xbrick",typekey=1601) 

        call prepare(lib_xbrick(1602),key=1602, & 
& nodecnc=[2567,1255,1381,2565,5139,3827,3953,5137,20446, 20445,20524, 20523,21189, 21190,20182, 20181 & 
& ,20448, 20447,20528, 20527,21191, 21192,20186, 20185], & 
& edgecnc=[7651,7690,8023,7519,7652,7692,8024,7521], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1602),elname="xbrick",eltype="xbrick",typekey=1602) 

        call prepare(lib_xbrick(1603),key=1603, & 
& nodecnc=[1347,1360,1364,214,3919,3932,3936,2786,11688, 11687,21193, 21194,21195, 21196,11666, 11665 & 
& ,11696, 11695,21197, 21198,21199, 21200,11674, 11673], & 
& edgecnc=[3272,8025,8026,3261,3276,8027,8028,3265], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1603),elname="xbrick",eltype="xbrick",typekey=1603) 

        call prepare(lib_xbrick(1604),key=1604, & 
& nodecnc=[2252,269,1361,1372,4824,2841,3933,3944,21201, 21202,20944, 20943,21118, 21117,21203, 21204 & 
& ,21205, 21206,20948, 20947,21122, 21121,21207, 21208], & 
& edgecnc=[8029,7900,7987,8030,8031,7902,7989,8032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1604),elname="xbrick",eltype="xbrick",typekey=1604) 

        call prepare(lib_xbrick(1605),key=1605, & 
& nodecnc=[2054,1373,147,1362,4626,3945,2719,3934,21209, 21210,9484, 9483,21042, 21041,21211, 21212,21213 & 
& , 21214,9492, 9491,21046, 21045,21215, 21216], & 
& edgecnc=[8033,2170,7949,8034,8035,2174,7951,8036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1605),elname="xbrick",eltype="xbrick",typekey=1605) 

        call prepare(lib_xbrick(1606),key=1606, & 
& nodecnc=[2512,2054,1362,1353,5084,4626,3934,3925,21217, 21218,21212, 21211,20950, 20949,8912, 8911,21219 & 
& , 21220,21216, 21215,20956, 20955,8920, 8919], & 
& edgecnc=[8037,8034,7903,1884,8038,8036,7906,1888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1606),elname="xbrick",eltype="xbrick",typekey=1606) 

        call prepare(lib_xbrick(1607),key=1607, & 
& nodecnc=[1668,349,1419,1626,4240,2921,3991,4198,21221, 21222,21223, 21224,12992, 12991,13006, 13005 & 
& ,21225, 21226,21227, 21228,12998, 12997,13014, 13013], & 
& edgecnc=[8039,8040,3924,3931,8041,8042,3927,3935], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1607),elname="xbrick",eltype="xbrick",typekey=1607) 

        call prepare(lib_xbrick(1608),key=1608, & 
& nodecnc=[349,1363,1428,1419,2921,3935,4000,3991,13072, 13071,8896, 8895,21229, 21230,21224, 21223,13080 & 
& , 13079,8904, 8903,21231, 21232,21228, 21227], & 
& edgecnc=[3964,1876,8043,8040,3968,1880,8044,8042], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1608),elname="xbrick",eltype="xbrick",typekey=1608) 

        call prepare(lib_xbrick(1609),key=1609, & 
& nodecnc=[2392,1364,180,1390,4964,3936,2752,3962,21233, 21234,21235, 21236,11520, 11519,21237, 21238 & 
& ,21239, 21240,21241, 21242,11526, 11525,21243, 21244], & 
& edgecnc=[8045,8046,3188,8047,8048,8049,3191,8050], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1609),elname="xbrick",eltype="xbrick",typekey=1609) 

        call prepare(lib_xbrick(1610),key=1610, & 
& nodecnc=[2392,1380,214,1364,4964,3952,2786,3936,11530, 11529,21245, 21246,21196, 21195,21234, 21233 & 
& ,11538, 11537,21247, 21248,21200, 21199,21240, 21239], & 
& edgecnc=[3193,8051,8026,8045,3197,8052,8028,8048], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1610),elname="xbrick",eltype="xbrick",typekey=1610) 

        call prepare(lib_xbrick(1611),key=1611, & 
& nodecnc=[1390,1379,1399,1424,3962,3951,3971,3996,11518, 11517,21249, 21250,21251, 21252,21253, 21254 & 
& ,11524, 11523,21255, 21256,21257, 21258,21259, 21260], & 
& edgecnc=[3187,8053,8054,8055,3190,8056,8057,8058], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1611),elname="xbrick",eltype="xbrick",typekey=1611) 

        call prepare(lib_xbrick(1612),key=1612, & 
& nodecnc=[2550,2530,1340,1647,5122,5102,3912,4219,21261, 21262,21030, 21029,21038, 21037,21018, 21017 & 
& ,21263, 21264,21036, 21035,21040, 21039,21024, 21023], & 
& edgecnc=[8059,7943,7947,7937,8060,7946,7948,7940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1612),elname="xbrick",eltype="xbrick",typekey=1612) 

        call prepare(lib_xbrick(1613),key=1613, & 
& nodecnc=[2231,1181,1149,1344,4803,3753,3721,3916,21078, 21077,19926, 19925,19796, 19795,11046, 11045 & 
& ,21084, 21083,19930, 19929,19800, 19799,11054, 11053], & 
& edgecnc=[7967,7391,7326,2951,7970,7393,7328,2955], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1613),elname="xbrick",eltype="xbrick",typekey=1613) 

        call prepare(lib_xbrick(1614),key=1614, & 
& nodecnc=[1865,2514,1930,2535,4437,5086,4502,5107,21265, 21266,21267, 21268,10334, 10333,21269, 21270 & 
& ,21271, 21272,21273, 21274,10342, 10341,21275, 21276], & 
& edgecnc=[8061,8062,2595,8063,8064,8065,2599,8066], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1614),elname="xbrick",eltype="xbrick",typekey=1614) 

        call prepare(lib_xbrick(1615),key=1615, & 
& nodecnc=[2566,1936,118,1368,5138,4508,2690,3940,21066, 21065,21277, 21278,10338, 10337,21279, 21280 & 
& ,21072, 21071,21281, 21282,10346, 10345,21283, 21284], & 
& edgecnc=[7961,8067,2597,8068,7964,8069,2601,8070], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1615),elname="xbrick",eltype="xbrick",typekey=1615) 

        call prepare(lib_xbrick(1616),key=1616, & 
& nodecnc=[2208,1386,471,76,4780,3958,3043,2648,8880, 8879,21285, 21286,21287, 21288,13864, 13863,8888 & 
& , 8887,21289, 21290,21291, 21292,13870, 13869], & 
& edgecnc=[1868,8071,8072,4360,1872,8073,8074,4363], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1616),elname="xbrick",eltype="xbrick",typekey=1616) 

        call prepare(lib_xbrick(1617),key=1617, & 
& nodecnc=[1364,1360,1357,180,3936,3932,3929,2752,21194, 21193,21166, 21165,21186, 21185,21236, 21235 & 
& ,21198, 21197,21168, 21167,21188, 21187,21242, 21241], & 
& edgecnc=[8025,8011,8021,8046,8027,8012,8022,8049], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1617),elname="xbrick",eltype="xbrick",typekey=1617) 

        call prepare(lib_xbrick(1618),key=1618, & 
& nodecnc=[1617,1382,2212,2211,4189,3954,4784,4783,21293, 21294,21295, 21296,21297, 21298,21299, 21300 & 
& ,21301, 21302,21303, 21304,21305, 21306,21307, 21308], & 
& edgecnc=[8075,8076,8077,8078,8079,8080,8081,8082], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1618),elname="xbrick",eltype="xbrick",typekey=1618) 

        call prepare(lib_xbrick(1619),key=1619, & 
& nodecnc=[212,2302,1335,1375,2784,4874,3907,3947,21309, 21310,20894, 20893,20726, 20725,20978, 20977 & 
& ,21311, 21312,20898, 20897,20732, 20731,20982, 20981], & 
& edgecnc=[8083,7875,7791,7917,8084,7877,7794,7919], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1619),elname="xbrick",eltype="xbrick",typekey=1619) 

        call prepare(lib_xbrick(1620),key=1620, & 
& nodecnc=[1383,1377,2252,1372,3955,3949,4824,3944,21313, 21314,11654, 11653,21204, 21203,21172, 21171 & 
& ,21315, 21316,11662, 11661,21208, 21207,21176, 21175], & 
& edgecnc=[8085,3255,8030,8014,8086,3259,8032,8016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1620),elname="xbrick",eltype="xbrick",typekey=1620) 

        call prepare(lib_xbrick(1621),key=1621, & 
& nodecnc=[101,1405,1373,2054,2673,3977,3945,4626,21317, 21318,10018, 10017,21210, 21209,21319, 21320 & 
& ,21321, 21322,10024, 10023,21214, 21213,21323, 21324], & 
& edgecnc=[8087,2437,8033,8088,8089,2440,8035,8090], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1621),elname="xbrick",eltype="xbrick",typekey=1621) 

        call prepare(lib_xbrick(1622),key=1622, & 
& nodecnc=[1375,1374,1397,1387,3947,3946,3969,3959,20724, 20723,21325, 21326,21327, 21328,20980, 20979 & 
& ,20730, 20729,21329, 21330,21331, 21332,20984, 20983], & 
& edgecnc=[7790,8091,8092,7918,7793,8093,8094,7920], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1622),elname="xbrick",eltype="xbrick",typekey=1622) 

        call prepare(lib_xbrick(1623),key=1623, & 
& nodecnc=[271,1623,1376,2289,2843,4195,3948,4861,21333, 21334,21335, 21336,21160, 21159,21337, 21338 & 
& ,21339, 21340,21341, 21342,21164, 21163,21343, 21344], & 
& edgecnc=[8095,8096,8008,8097,8098,8099,8010,8100], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1623),elname="xbrick",eltype="xbrick",typekey=1623) 

        call prepare(lib_xbrick(1624),key=1624, & 
& nodecnc=[1376,1623,1388,236,3948,4195,3960,2808,21336, 21335,11000, 10999,11606, 11605,21150, 21149 & 
& ,21342, 21341,11008, 11007,11614, 11613,21156, 21155], & 
& edgecnc=[8096,2928,3231,8003,8099,2932,3235,8006], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1624),elname="xbrick",eltype="xbrick",typekey=1624) 

        call prepare(lib_xbrick(1625),key=1625, & 
& nodecnc=[2230,1401,1377,1455,4802,3973,3949,4027,21345, 21346,11656, 11655,21347, 21348,21349, 21350 & 
& ,21351, 21352,11664, 11663,21353, 21354,21355, 21356], & 
& edgecnc=[8101,3256,8102,8103,8104,3260,8105,8106], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1625),elname="xbrick",eltype="xbrick",typekey=1625) 

        call prepare(lib_xbrick(1626),key=1626, & 
& nodecnc=[1455,1377,1383,268,4027,3949,3955,2840,21348, 21347,21314, 21313,21357, 21358,12246, 12245 & 
& ,21354, 21353,21316, 21315,21359, 21360,12254, 12253], & 
& edgecnc=[8102,8085,8107,3551,8105,8086,8108,3555], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1626),elname="xbrick",eltype="xbrick",typekey=1626) 

        call prepare(lib_xbrick(1627),key=1627, & 
& nodecnc=[1391,1857,1399,1379,3963,4429,3971,3951,21361, 21362,21363, 21364,21250, 21249,10816, 10815 & 
& ,21365, 21366,21367, 21368,21256, 21255,10824, 10823], & 
& edgecnc=[8109,8110,8053,2836,8111,8112,8056,2840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1627),elname="xbrick",eltype="xbrick",typekey=1627) 

        call prepare(lib_xbrick(1628),key=1628, & 
& nodecnc=[1403,214,1380,1394,3975,2786,3952,3966,11668, 11667,21246, 21245,21369, 21370,21371, 21372 & 
& ,11676, 11675,21248, 21247,21373, 21374,21375, 21376], & 
& edgecnc=[3262,8051,8113,8114,3266,8052,8115,8116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1628),elname="xbrick",eltype="xbrick",typekey=1628) 

        call prepare(lib_xbrick(1629),key=1629, & 
& nodecnc=[215,2286,1402,2333,2787,4858,3974,4905,21377, 21378,21379, 21380,21381, 21382,21383, 21384 & 
& ,21385, 21386,21387, 21388,21389, 21390,21391, 21392], & 
& edgecnc=[8117,8118,8119,8120,8121,8122,8123,8124], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1629),elname="xbrick",eltype="xbrick",typekey=1629) 

        call prepare(lib_xbrick(1630),key=1630, & 
& nodecnc=[118,1936,2555,1882,2690,4508,5127,4454,21278, 21277,21393, 21394,20522, 20521,21395, 21396 & 
& ,21282, 21281,21397, 21398,20526, 20525,21399, 21400], & 
& edgecnc=[8067,8125,7689,8126,8069,8127,7691,8128], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1630),elname="xbrick",eltype="xbrick",typekey=1630) 

        call prepare(lib_xbrick(1631),key=1631, & 
& nodecnc=[2565,1381,98,1205,5137,3953,2670,3777,21190, 21189,9904, 9903,21401, 21402,20184, 20183,21192 & 
& , 21191,9912, 9911,21403, 21404,20188, 20187], & 
& edgecnc=[8023,2380,8129,7520,8024,2384,8130,7522], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1631),elname="xbrick",eltype="xbrick",typekey=1631) 

        call prepare(lib_xbrick(1632),key=1632, & 
& nodecnc=[1622,1382,403,39,4194,3954,2975,2611,8424, 8423,21405, 21406,21407, 21408,21409, 21410,8432 & 
& , 8431,21411, 21412,21413, 21414,21415, 21416], & 
& edgecnc=[1640,8131,8132,8133,1644,8134,8135,8136], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1632),elname="xbrick",eltype="xbrick",typekey=1632) 

        call prepare(lib_xbrick(1633),key=1633, & 
& nodecnc=[1403,1394,268,1383,3975,3966,2840,3955,21372, 21371,21417, 21418,21358, 21357,21170, 21169 & 
& ,21376, 21375,21419, 21420,21360, 21359,21174, 21173], & 
& edgecnc=[8114,8137,8107,8013,8116,8138,8108,8015], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1633),elname="xbrick",eltype="xbrick",typekey=1633) 

        call prepare(lib_xbrick(1634),key=1634, & 
& nodecnc=[1396,148,1404,1384,3968,2720,3976,3956,10030, 10029,21421, 21422,21423, 21424,10022, 10021 & 
& ,10036, 10035,21425, 21426,21427, 21428,10028, 10027], & 
& edgecnc=[2443,8139,8140,2439,2446,8141,8142,2442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1634),elname="xbrick",eltype="xbrick",typekey=1634) 

        call prepare(lib_xbrick(1635),key=1635, & 
& nodecnc=[347,1797,1463,1462,2919,4369,4035,4034,21429, 21430,21431, 21432,5604, 5603,21433, 21434,21435 & 
& , 21436,21437, 21438,5612, 5611,21439, 21440], & 
& edgecnc=[8143,8144,230,8145,8146,8147,234,8148], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1635),elname="xbrick",eltype="xbrick",typekey=1635) 

        call prepare(lib_xbrick(1636),key=1636, & 
& nodecnc=[1797,1385,1496,1463,4369,3957,4068,4035,21441, 21442,21443, 21444,5614, 5613,21432, 21431,21445 & 
& , 21446,21447, 21448,5620, 5619,21438, 21437], & 
& edgecnc=[8149,8150,235,8144,8151,8152,238,8147], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1636),elname="xbrick",eltype="xbrick",typekey=1636) 

        call prepare(lib_xbrick(1637),key=1637, & 
& nodecnc=[1386,1640,472,471,3958,4212,3044,3043,21449, 21450,13876, 13875,21451, 21452,21286, 21285,21453 & 
& , 21454,13884, 13883,21455, 21456,21290, 21289], & 
& edgecnc=[8153,4366,8154,8071,8155,4370,8156,8073], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1637),elname="xbrick",eltype="xbrick",typekey=1637) 

        call prepare(lib_xbrick(1638),key=1638, & 
& nodecnc=[349,1640,1386,1355,2921,4212,3958,3927,21457, 21458,21450, 21449,8878, 8877,13074, 13073,21459 & 
& , 21460,21454, 21453,8886, 8885,13082, 13081], & 
& edgecnc=[8157,8153,1867,3965,8158,8155,1871,3969], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1638),elname="xbrick",eltype="xbrick",typekey=1638) 

        call prepare(lib_xbrick(1639),key=1639, & 
& nodecnc=[1397,1406,270,1387,3969,3978,2842,3959,11592, 11591,21461, 21462,20876, 20875,21328, 21327 & 
& ,11600, 11599,21463, 21464,20882, 20881,21332, 21331], & 
& edgecnc=[3224,8159,7866,8092,3228,8160,7869,8094], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1639),elname="xbrick",eltype="xbrick",typekey=1639) 

        call prepare(lib_xbrick(1640),key=1640, & 
& nodecnc=[1415,1650,270,1406,3987,4222,2842,3978,21465, 21466,21467, 21468,21462, 21461,12278, 12277 & 
& ,21469, 21470,21471, 21472,21464, 21463,12284, 12283], & 
& edgecnc=[8161,8162,8159,3567,8163,8164,8160,3570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1640),elname="xbrick",eltype="xbrick",typekey=1640) 

        call prepare(lib_xbrick(1641),key=1641, & 
& nodecnc=[1374,269,1400,1397,3946,2841,3972,3969,20942, 20941,21473, 21474,11594, 11593,21326, 21325 & 
& ,20946, 20945,21475, 21476,11602, 11601,21330, 21329], & 
& edgecnc=[7899,8165,3225,8091,7901,8166,3229,8093], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1641),elname="xbrick",eltype="xbrick",typekey=1641) 

        call prepare(lib_xbrick(1642),key=1642, & 
& nodecnc=[1717,1414,1389,1401,4289,3986,3961,3973,11640, 11639,12288, 12287,11650, 11649,21477, 21478 & 
& ,11646, 11645,12294, 12293,11658, 11657,21479, 21480], & 
& edgecnc=[3248,3572,3253,8167,3251,3575,3257,8168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1642),elname="xbrick",eltype="xbrick",typekey=1642) 

        call prepare(lib_xbrick(1643),key=1643, & 
& nodecnc=[1424,1393,2392,1390,3996,3965,4964,3962,21481, 21482,11532, 11531,21238, 21237,21254, 21253 & 
& ,21483, 21484,11540, 11539,21244, 21243,21260, 21259], & 
& edgecnc=[8169,3194,8047,8055,8170,3198,8050,8058], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1643),elname="xbrick",eltype="xbrick",typekey=1643) 

        call prepare(lib_xbrick(1644),key=1644, & 
& nodecnc=[164,1857,1391,1644,2736,4429,3963,4216,21485, 21486,21362, 21361,11702, 11701,21487, 21488 & 
& ,21489, 21490,21366, 21365,11708, 11707,21491, 21492], & 
& edgecnc=[8171,8109,3279,8172,8173,8111,3282,8174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1644),elname="xbrick",eltype="xbrick",typekey=1644) 

        call prepare(lib_xbrick(1645),key=1645, & 
& nodecnc=[2333,1393,1417,215,4905,3965,3989,2787,11534, 11533,21493, 21494,21495, 21496,21384, 21383 & 
& ,11542, 11541,21497, 21498,21499, 21500,21392, 21391], & 
& edgecnc=[3195,8175,8176,8120,3199,8177,8178,8124], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1645),elname="xbrick",eltype="xbrick",typekey=1645) 

        call prepare(lib_xbrick(1646),key=1646, & 
& nodecnc=[267,2234,1453,2291,2839,4806,4025,4863,7340, 7339,21501, 21502,12250, 12249,21503, 21504,7348 & 
& , 7347,21505, 21506,12258, 12257,21507, 21508], & 
& edgecnc=[1098,8179,3553,8180,1102,8181,3557,8182], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1646),elname="xbrick",eltype="xbrick",typekey=1646) 

        call prepare(lib_xbrick(1647),key=1647, & 
& nodecnc=[1384,1404,1410,1395,3956,3976,3982,3967,21424, 21423,21509, 21510,9502, 9501,9488, 9487,21428 & 
& , 21427,21511, 21512,9510, 9509,9496, 9495], & 
& edgecnc=[8140,8183,2179,2172,8142,8184,2183,2176], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1647),elname="xbrick",eltype="xbrick",typekey=1647) 

        call prepare(lib_xbrick(1648),key=1648, & 
& nodecnc=[1968,102,1396,1405,4540,2674,3968,3977,21513, 21514,10032, 10031,10020, 10019,21515, 21516 & 
& ,21517, 21518,10038, 10037,10026, 10025,21519, 21520], & 
& edgecnc=[8185,2444,2438,8186,8187,2447,2441,8188], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1648),elname="xbrick",eltype="xbrick",typekey=1648) 

        call prepare(lib_xbrick(1649),key=1649, & 
& nodecnc=[271,1346,2255,1623,2843,3918,4827,4195,20864, 20863,21521, 21522,10994, 10993,21334, 21333 & 
& ,20870, 20869,21523, 21524,11002, 11001,21340, 21339], & 
& edgecnc=[7860,8189,2925,8095,7863,8190,2929,8098], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1649),elname="xbrick",eltype="xbrick",typekey=1649) 

        call prepare(lib_xbrick(1650),key=1650, & 
& nodecnc=[1857,1407,181,1399,4429,3979,2753,3971,21525, 21526,11876, 11875,21527, 21528,21364, 21363 & 
& ,21529, 21530,11880, 11879,21531, 21532,21368, 21367], & 
& edgecnc=[8191,3366,8192,8110,8193,3368,8194,8112], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1650),elname="xbrick",eltype="xbrick",typekey=1650) 

        call prepare(lib_xbrick(1651),key=1651, & 
& nodecnc=[269,2252,1389,1400,2841,4824,3961,3972,21202, 21201,11652, 11651,12286, 12285,21474, 21473 & 
& ,21206, 21205,11660, 11659,12292, 12291,21476, 21475], & 
& edgecnc=[8029,3254,3571,8165,8031,3258,3574,8166], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1651),elname="xbrick",eltype="xbrick",typekey=1651) 

        call prepare(lib_xbrick(1652),key=1652, & 
& nodecnc=[1703,267,2286,1653,4275,2839,4858,4225,7342, 7341,21533, 21534,21535, 21536,21537, 21538,7350 & 
& , 7349,21539, 21540,21541, 21542,21543, 21544], & 
& edgecnc=[1099,8195,8196,8197,1103,8198,8199,8200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1652),elname="xbrick",eltype="xbrick",typekey=1652) 

        call prepare(lib_xbrick(1653),key=1653, & 
& nodecnc=[2333,1402,1394,1380,4905,3974,3966,3952,21382, 21381,21545, 21546,21370, 21369,11536, 11535 & 
& ,21390, 21389,21547, 21548,21374, 21373,11544, 11543], & 
& edgecnc=[8119,8201,8113,3196,8123,8202,8115,3200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1653),elname="xbrick",eltype="xbrick",typekey=1653) 

        call prepare(lib_xbrick(1654),key=1654, & 
& nodecnc=[2348,2332,2335,2303,4920,4904,4907,4875,9514, 9513,21549, 21550,21551, 21552,21553, 21554,9522 & 
& , 9521,21555, 21556,21557, 21558,21559, 21560], & 
& edgecnc=[2185,8203,8204,8205,2189,8206,8207,8208], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1654),elname="xbrick",eltype="xbrick",typekey=1654) 

        call prepare(lib_xbrick(1655),key=1655, & 
& nodecnc=[2024,1915,102,1968,4596,4487,2674,4540,10004, 10003,10068, 10067,21514, 21513,21561, 21562 & 
& ,10012, 10011,10076, 10075,21518, 21517,21563, 21564], & 
& edgecnc=[2430,2462,8185,8209,2434,2466,8187,8210], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1655),elname="xbrick",eltype="xbrick",typekey=1655) 

        call prepare(lib_xbrick(1656),key=1656, & 
& nodecnc=[164,1425,1407,1857,2736,3997,3979,4429,11716, 11715,7206, 7205,21526, 21525,21486, 21485,11724 & 
& , 11723,7214, 7213,21530, 21529,21490, 21489], & 
& edgecnc=[3286,1031,8191,8171,3290,1035,8193,8173], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1656),elname="xbrick",eltype="xbrick",typekey=1656) 

        call prepare(lib_xbrick(1657),key=1657, & 
& nodecnc=[1651,1617,2211,1408,4223,4189,4783,3980,11014, 11013,21300, 21299,11028, 11027,13356, 13355 & 
& ,11022, 11021,21308, 21307,11036, 11035,13362, 13361], & 
& edgecnc=[2935,8078,2942,4106,2939,8082,2946,4109], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1657),elname="xbrick",eltype="xbrick",typekey=1657) 

        call prepare(lib_xbrick(1658),key=1658, & 
& nodecnc=[2286,267,2291,1402,4858,2839,4863,3974,21534, 21533,21504, 21503,21565, 21566,21380, 21379 & 
& ,21540, 21539,21508, 21507,21567, 21568,21388, 21387], & 
& edgecnc=[8195,8180,8211,8118,8198,8182,8212,8122], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1658),elname="xbrick",eltype="xbrick",typekey=1658) 

        call prepare(lib_xbrick(1659),key=1659, & 
& nodecnc=[2332,1413,315,1410,4904,3985,2887,3982,9520, 9519,21569, 21570,9504, 9503,21571, 21572,9528 & 
& , 9527,21573, 21574,9512, 9511,21575, 21576], & 
& edgecnc=[2188,8213,2180,8214,2192,8215,2184,8216], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1659),elname="xbrick",eltype="xbrick",typekey=1659) 

        call prepare(lib_xbrick(1660),key=1660, & 
& nodecnc=[2335,2332,1410,1404,4907,4904,3982,3976,21550, 21549,21572, 21571,21510, 21509,21577, 21578 & 
& ,21556, 21555,21576, 21575,21512, 21511,21579, 21580], & 
& edgecnc=[8203,8214,8183,8217,8206,8216,8184,8218], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1660),elname="xbrick",eltype="xbrick",typekey=1660) 

        call prepare(lib_xbrick(1661),key=1661, & 
& nodecnc=[1416,1411,1409,1414,3988,3983,3981,3986,21581, 21582,12274, 12273,12290, 12289,11638, 11637 & 
& ,21583, 21584,12280, 12279,12296, 12295,11644, 11643], & 
& edgecnc=[8219,3565,3573,3247,8220,3568,3576,3250], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1661),elname="xbrick",eltype="xbrick",typekey=1661) 

        call prepare(lib_xbrick(1662),key=1662, & 
& nodecnc=[1416,1427,1434,1411,3988,3999,4006,3983,13382, 13381,13366, 13365,21585, 21586,21582, 21581 & 
& ,13388, 13387,13372, 13371,21587, 21588,21584, 21583], & 
& edgecnc=[4119,4111,8221,8219,4122,4114,8222,8220], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1662),elname="xbrick",eltype="xbrick",typekey=1662) 

        call prepare(lib_xbrick(1663),key=1663, & 
& nodecnc=[1648,1700,215,1417,4220,4272,2787,3989,21589, 21590,21591, 21592,21496, 21495,10844, 10843 & 
& ,21593, 21594,21595, 21596,21500, 21499,10852, 10851], & 
& edgecnc=[8223,8224,8176,2850,8225,8226,8178,2854], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1663),elname="xbrick",eltype="xbrick",typekey=1663) 

        call prepare(lib_xbrick(1664),key=1664, & 
& nodecnc=[1428,315,1413,1419,4000,2887,3985,3991,20842, 20841,21570, 21569,12994, 12993,21230, 21229 & 
& ,20846, 20845,21574, 21573,13000, 12999,21232, 21231], & 
& edgecnc=[7849,8213,3925,8043,7851,8215,3928,8044], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1664),elname="xbrick",eltype="xbrick",typekey=1664) 

        call prepare(lib_xbrick(1665),key=1665, & 
& nodecnc=[1626,2298,316,1654,4198,4870,2888,4226,12990, 12989,21597, 21598,12832, 12831,13008, 13007 & 
& ,12996, 12995,21599, 21600,12840, 12839,13016, 13015], & 
& edgecnc=[3923,8227,3844,3932,3926,8228,3848,3936], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1665),elname="xbrick",eltype="xbrick",typekey=1665) 

        call prepare(lib_xbrick(1666),key=1666, & 
& nodecnc=[1411,1434,235,1415,3983,4006,2807,3987,21586, 21585,12298, 12297,21601, 21602,12276, 12275 & 
& ,21588, 21587,12302, 12301,21603, 21604,12282, 12281], & 
& edgecnc=[8221,3577,8229,3566,8222,3579,8230,3569], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1666),elname="xbrick",eltype="xbrick",typekey=1666) 

        call prepare(lib_xbrick(1667),key=1667, & 
& nodecnc=[2505,1417,1393,1424,5077,3989,3965,3996,10846, 10845,21494, 21493,21482, 21481,21605, 21606 & 
& ,10854, 10853,21498, 21497,21484, 21483,21607, 21608], & 
& edgecnc=[2851,8175,8169,8231,2855,8177,8170,8232], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1667),elname="xbrick",eltype="xbrick",typekey=1667) 

        call prepare(lib_xbrick(1668),key=1668, & 
& nodecnc=[1628,2505,181,1412,4200,5077,2753,3984,10848, 10847,21609, 21610,11874, 11873,21611, 21612 & 
& ,10856, 10855,21613, 21614,11878, 11877,21615, 21616], & 
& edgecnc=[2852,8233,3365,8234,2856,8235,3367,8236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1668),elname="xbrick",eltype="xbrick",typekey=1668) 

        call prepare(lib_xbrick(1669),key=1669, & 
& nodecnc=[2132,1610,182,2529,4704,4182,2754,5101,21617, 21618,11884, 11883,10828, 10827,21619, 21620 & 
& ,21621, 21622,11892, 11891,10836, 10835,21623, 21624], & 
& edgecnc=[8237,3370,2842,8238,8239,3374,2846,8240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1669),elname="xbrick",eltype="xbrick",typekey=1669) 

        call prepare(lib_xbrick(1670),key=1670, & 
& nodecnc=[1239,1221,338,1423,3811,3793,2910,3995,20276, 20275,12722, 12721,10900, 10899,20400, 20399 & 
& ,20280, 20279,12728, 12727,10906, 10905,20406, 20405], & 
& edgecnc=[7566,3789,2878,7628,7568,3792,2881,7631], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1670),elname="xbrick",eltype="xbrick",typekey=1670) 

        call prepare(lib_xbrick(1671),key=1671, & 
& nodecnc=[1429,1578,337,1420,4001,4150,2909,3992,13736, 13735,21625, 21626,11134, 11133,8556, 8555,13742 & 
& , 13741,21627, 21628,11140, 11139,8564, 8563], & 
& edgecnc=[4296,8241,2995,1706,4299,8242,2998,1710], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1671),elname="xbrick",eltype="xbrick",typekey=1671) 

        call prepare(lib_xbrick(1672),key=1672, & 
& nodecnc=[1385,1797,1706,1421,3957,4369,4278,3993,21442, 21441,21629, 21630,21631, 21632,13854, 13853 & 
& ,21446, 21445,21633, 21634,21635, 21636,13860, 13859], & 
& edgecnc=[8149,8243,8244,4355,8151,8245,8246,4358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1672),elname="xbrick",eltype="xbrick",typekey=1672) 

        call prepare(lib_xbrick(1673),key=1673, & 
& nodecnc=[2329,1242,2381,1649,4901,3814,4953,4221,20418, 20417,20296, 20295,20384, 20383,21637, 21638 & 
& ,20424, 20423,20302, 20301,20388, 20387,21639, 21640], & 
& edgecnc=[7637,7576,7620,8247,7640,7579,7622,8248], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1673),elname="xbrick",eltype="xbrick",typekey=1673) 

        call prepare(lib_xbrick(1674),key=1674, & 
& nodecnc=[2505,1424,1399,181,5077,3996,3971,2753,21606, 21605,21252, 21251,21528, 21527,21610, 21609 & 
& ,21608, 21607,21258, 21257,21532, 21531,21614, 21613], & 
& edgecnc=[8231,8054,8192,8233,8232,8057,8194,8235], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1674),elname="xbrick",eltype="xbrick",typekey=1674) 

        call prepare(lib_xbrick(1675),key=1675, & 
& nodecnc=[2335,1404,148,1433,4907,3976,2720,4005,21578, 21577,21422, 21421,9470, 9469,21641, 21642,21580 & 
& , 21579,21426, 21425,9478, 9477,21643, 21644], & 
& edgecnc=[8217,8139,2163,8249,8218,8141,2167,8250], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1675),elname="xbrick",eltype="xbrick",typekey=1675) 

        call prepare(lib_xbrick(1676),key=1676, & 
& nodecnc=[1438,1427,41,406,4010,3999,2613,2978,13368, 13367,13380, 13379,21645, 21646,21647, 21648,13374 & 
& , 13373,13386, 13385,21649, 21650,21651, 21652], & 
& edgecnc=[4112,4118,8251,8252,4115,4121,8253,8254], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1676),elname="xbrick",eltype="xbrick",typekey=1676) 

        call prepare(lib_xbrick(1677),key=1677, & 
& nodecnc=[1541,1730,337,1578,4113,4302,2909,4150,21653, 21654,21655, 21656,21626, 21625,8222, 8221,21657 & 
& , 21658,21659, 21660,21628, 21627,8230, 8229], & 
& edgecnc=[8255,8256,8241,1539,8257,8258,8242,1543], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1677),elname="xbrick",eltype="xbrick",typekey=1677) 

        call prepare(lib_xbrick(1678),key=1678, & 
& nodecnc=[337,1632,303,1430,2909,4204,2875,4002,21661, 21662,20674, 20673,20414, 20413,11136, 11135,21663 & 
& , 21664,20678, 20677,20420, 20419,11142, 11141], & 
& edgecnc=[8259,7765,7635,2996,8260,7767,7638,2999], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1678),elname="xbrick",eltype="xbrick",typekey=1678) 

        call prepare(lib_xbrick(1679),key=1679, & 
& nodecnc=[1341,2359,1431,1327,3913,4931,4003,3899,21665, 21666,20920, 20919,20844, 20843,12982, 12981 & 
& ,21667, 21668,20924, 20923,20848, 20847,12986, 12985], & 
& edgecnc=[8261,7888,7850,3919,8262,7890,7852,3921], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1679),elname="xbrick",eltype="xbrick",typekey=1679) 

        call prepare(lib_xbrick(1680),key=1680, & 
& nodecnc=[1721,1807,1785,103,4293,4379,4357,2675,21669, 21670,21671, 21672,10056, 10055,21673, 21674 & 
& ,21675, 21676,21677, 21678,10062, 10061,21679, 21680], & 
& edgecnc=[8263,8264,2456,8265,8266,8267,2459,8268], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1680),elname="xbrick",eltype="xbrick",typekey=1680) 

        call prepare(lib_xbrick(1681),key=1681, & 
& nodecnc=[1439,1426,1422,1432,4011,3998,3994,4004,10042, 10041,9466, 9465,21681, 21682,21683, 21684,10048 & 
& , 10047,9474, 9473,21685, 21686,21687, 21688], & 
& edgecnc=[2449,2161,8269,8270,2452,2165,8271,8272], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1681),elname="xbrick",eltype="xbrick",typekey=1681) 

        call prepare(lib_xbrick(1682),key=1682, & 
& nodecnc=[1440,2335,1433,1436,4012,4907,4005,4008,21689, 21690,21642, 21641,11380, 11379,21691, 21692 & 
& ,21693, 21694,21644, 21643,11384, 11383,21695, 21696], & 
& edgecnc=[8273,8249,3118,8274,8275,8250,3120,8276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1682),elname="xbrick",eltype="xbrick",typekey=1682) 

        call prepare(lib_xbrick(1683),key=1683, & 
& nodecnc=[1438,1441,1398,1435,4010,4013,3970,4007,21697, 21698,13358, 13357,11610, 11609,13370, 13369 & 
& ,21699, 21700,13364, 13363,11618, 11617,13376, 13375], & 
& edgecnc=[8277,4107,3233,4113,8278,4110,3237,4116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1683),elname="xbrick",eltype="xbrick",typekey=1683) 

        call prepare(lib_xbrick(1684),key=1684, & 
& nodecnc=[317,1440,1436,1437,2889,4012,4008,4009,21701, 21702,21692, 21691,11388, 11387,9456, 9455,21703 & 
& , 21704,21696, 21695,11394, 11393,9464, 9463], & 
& edgecnc=[8279,8274,3122,2156,8280,8276,3125,2160], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1684),elname="xbrick",eltype="xbrick",typekey=1684) 

        call prepare(lib_xbrick(1685),key=1685, & 
& nodecnc=[1443,1444,1447,1437,4015,4016,4019,4009,21705, 21706,21707, 21708,9450, 9449,11386, 11385,21709 & 
& , 21710,21711, 21712,9458, 9457,11392, 11391], & 
& edgecnc=[8281,8282,2153,3121,8283,8284,2157,3124], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1685),elname="xbrick",eltype="xbrick",typekey=1685) 

        call prepare(lib_xbrick(1686),key=1686, & 
& nodecnc=[1443,1445,150,1444,4015,4017,2722,4016,11398, 11397,21713, 21714,21715, 21716,21706, 21705 & 
& ,11402, 11401,21717, 21718,21719, 21720,21710, 21709], & 
& edgecnc=[3127,8285,8286,8281,3129,8287,8288,8283], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1686),elname="xbrick",eltype="xbrick",typekey=1686) 

        call prepare(lib_xbrick(1687),key=1687, & 
& nodecnc=[1441,1438,406,405,4013,4010,2978,2977,21698, 21697,21648, 21647,21721, 21722,21723, 21724,21700 & 
& , 21699,21652, 21651,21725, 21726,21727, 21728], & 
& edgecnc=[8277,8252,8289,8290,8278,8254,8291,8292], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1687),elname="xbrick",eltype="xbrick",typekey=1687) 

        call prepare(lib_xbrick(1688),key=1688, & 
& nodecnc=[1807,1721,1439,1432,4379,4293,4011,4004,21670, 21669,9322, 9321,21684, 21683,21729, 21730,21676 & 
& , 21675,9330, 9329,21688, 21687,21731, 21732], & 
& edgecnc=[8263,2089,8270,8293,8266,2093,8272,8294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1688),elname="xbrick",eltype="xbrick",typekey=1688) 

        call prepare(lib_xbrick(1689),key=1689, & 
& nodecnc=[1440,317,1451,1456,4012,2889,4023,4028,21702, 21701,21733, 21734,21735, 21736,21737, 21738 & 
& ,21704, 21703,21739, 21740,21741, 21742,21743, 21744], & 
& edgecnc=[8279,8295,8296,8297,8280,8298,8299,8300], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1689),elname="xbrick",eltype="xbrick",typekey=1689) 

        call prepare(lib_xbrick(1690),key=1690, & 
& nodecnc=[1651,1441,405,40,4223,4013,2977,2612,13354, 13353,21724, 21723,21745, 21746,11016, 11015,13360 & 
& , 13359,21728, 21727,21747, 21748,11024, 11023], & 
& edgecnc=[4105,8290,8301,2936,4108,8292,8302,2940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1690),elname="xbrick",eltype="xbrick",typekey=1690) 

        call prepare(lib_xbrick(1691),key=1691, & 
& nodecnc=[2451,1448,1445,1442,5023,4020,4017,4014,21749, 21750,21751, 21752,11400, 11399,9326, 9325,21753 & 
& , 21754,21755, 21756,11404, 11403,9334, 9333], & 
& edgecnc=[8303,8304,3128,2091,8305,8306,3130,2095], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1691),elname="xbrick",eltype="xbrick",typekey=1691) 

        call prepare(lib_xbrick(1692),key=1692, & 
& nodecnc=[1454,1444,150,2428,4026,4016,2722,5000,21757, 21758,21716, 21715,10122, 10121,21759, 21760 & 
& ,21761, 21762,21720, 21719,10128, 10127,21763, 21764], & 
& edgecnc=[8307,8286,2489,8308,8309,8288,2492,8310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1692),elname="xbrick",eltype="xbrick",typekey=1692) 

        call prepare(lib_xbrick(1693),key=1693, & 
& nodecnc=[1448,1446,150,1445,4020,4018,2722,4017,21765, 21766,10124, 10123,21714, 21713,21752, 21751 & 
& ,21767, 21768,10130, 10129,21718, 21717,21756, 21755], & 
& edgecnc=[8311,2490,8285,8304,8312,2493,8287,8306], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1693),elname="xbrick",eltype="xbrick",typekey=1693) 

        call prepare(lib_xbrick(1694),key=1694, & 
& nodecnc=[1637,1446,1448,104,4209,4018,4020,2676,10134, 10133,21766, 21765,21769, 21770,9210, 9209,10136 & 
& , 10135,21768, 21767,21771, 21772,9216, 9215], & 
& edgecnc=[2495,8311,8313,2033,2496,8312,8314,2036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1694),elname="xbrick",eltype="xbrick",typekey=1694) 

        call prepare(lib_xbrick(1695),key=1695, & 
& nodecnc=[1457,1451,317,1449,4029,4023,2889,4021,21773, 21774,21734, 21733,9454, 9453,21775, 21776,21777 & 
& , 21778,21740, 21739,9462, 9461,21779, 21780], & 
& edgecnc=[8315,8295,2155,8316,8317,8298,2159,8318], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1695),elname="xbrick",eltype="xbrick",typekey=1695) 

        call prepare(lib_xbrick(1696),key=1696, & 
& nodecnc=[1454,318,1447,1444,4026,2890,4019,4016,21781, 21782,12848, 12847,21708, 21707,21758, 21757 & 
& ,21783, 21784,12854, 12853,21712, 21711,21762, 21761], & 
& edgecnc=[8319,3852,8282,8307,8320,3855,8284,8309], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1696),elname="xbrick",eltype="xbrick",typekey=1696) 

        call prepare(lib_xbrick(1697),key=1697, & 
& nodecnc=[1448,2451,1627,104,4020,5023,4199,2676,21750, 21749,21785, 21786,10104, 10103,21770, 21769 & 
& ,21754, 21753,21787, 21788,10108, 10107,21772, 21771], & 
& edgecnc=[8303,8321,2480,8313,8305,8322,2482,8314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1697),elname="xbrick",eltype="xbrick",typekey=1697) 

        call prepare(lib_xbrick(1698),key=1698, & 
& nodecnc=[1627,2451,1721,103,4199,5023,4293,2675,21786, 21785,9324, 9323,21674, 21673,10094, 10093,21788 & 
& , 21787,9332, 9331,21680, 21679,10100, 10099], & 
& edgecnc=[8321,2090,8265,2475,8322,2094,8268,2478], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1698),elname="xbrick",eltype="xbrick",typekey=1698) 

        call prepare(lib_xbrick(1699),key=1699, & 
& nodecnc=[1458,1457,1449,1459,4030,4029,4021,4031,13910, 13909,21776, 21775,12846, 12845,21789, 21790 & 
& ,13916, 13915,21780, 21779,12852, 12851,21791, 21792], & 
& edgecnc=[4383,8316,3851,8323,4386,8318,3854,8324], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1699),elname="xbrick",eltype="xbrick",typekey=1699) 

        call prepare(lib_xbrick(1700),key=1700, & 
& nodecnc=[1613,1451,1457,351,4185,4023,4029,2923,21793, 21794,21774, 21773,13914, 13913,12822, 12821 & 
& ,21795, 21796,21778, 21777,13920, 13919,12828, 12827], & 
& edgecnc=[8325,8315,4385,3839,8326,8317,4388,3842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1700),elname="xbrick",eltype="xbrick",typekey=1700) 

        call prepare(lib_xbrick(1701),key=1701, & 
& nodecnc=[1629,1753,1454,2428,4201,4325,4026,5000,21797, 21798,21799, 21800,21760, 21759,9340, 9339,21801 & 
& , 21802,21803, 21804,21764, 21763,9346, 9345], & 
& edgecnc=[8327,8328,8308,2098,8329,8330,8310,2101], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1701),elname="xbrick",eltype="xbrick",typekey=1701) 

        call prepare(lib_xbrick(1702),key=1702, & 
& nodecnc=[2230,1455,1453,1625,4802,4027,4025,4197,21350, 21349,12252, 12251,21805, 21806,21807, 21808 & 
& ,21356, 21355,12260, 12259,21809, 21810,21811, 21812], & 
& edgecnc=[8103,3554,8331,8332,8106,3558,8333,8334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1702),elname="xbrick",eltype="xbrick",typekey=1702) 

        call prepare(lib_xbrick(1703),key=1703, & 
& nodecnc=[2234,1655,1625,1453,4806,4227,4197,4025,21813, 21814,21815, 21816,21806, 21805,21502, 21501 & 
& ,21817, 21818,21819, 21820,21810, 21809,21506, 21505], & 
& edgecnc=[8335,8336,8331,8179,8337,8338,8333,8181], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1703),elname="xbrick",eltype="xbrick",typekey=1703) 

        call prepare(lib_xbrick(1704),key=1704, & 
& nodecnc=[1661,318,1454,1753,4233,2890,4026,4325,12862, 12861,21782, 21781,21800, 21799,21821, 21822 & 
& ,12868, 12867,21784, 21783,21804, 21803,21823, 21824], & 
& edgecnc=[3859,8319,8328,8339,3862,8320,8330,8340], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1704),elname="xbrick",eltype="xbrick",typekey=1704) 

        call prepare(lib_xbrick(1705),key=1705, & 
& nodecnc=[2335,1440,1456,2303,4907,4012,4028,4875,21690, 21689,21738, 21737,21825, 21826,21552, 21551 & 
& ,21694, 21693,21744, 21743,21827, 21828,21558, 21557], & 
& edgecnc=[8273,8297,8341,8204,8275,8300,8342,8207], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1705),elname="xbrick",eltype="xbrick",typekey=1705) 

        call prepare(lib_xbrick(1706),key=1706, & 
& nodecnc=[2222,350,1654,1669,4794,2922,4226,4241,21829, 21830,13002, 13001,12830, 12829,21831, 21832 & 
& ,21833, 21834,13010, 13009,12838, 12837,21835, 21836], & 
& edgecnc=[8343,3929,3843,8344,8345,3933,3847,8346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1706),elname="xbrick",eltype="xbrick",typekey=1706) 

        call prepare(lib_xbrick(1707),key=1707, & 
& nodecnc=[476,79,1503,1474,3048,2651,4075,4046,21837, 21838,9552, 9551,13922, 13921,9440, 9439,21839 & 
& , 21840,9560, 9559,13928, 13927,9448, 9447], & 
& edgecnc=[8347,2204,4389,2148,8348,2208,4392,2152], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1707),elname="xbrick",eltype="xbrick",typekey=1707) 

        call prepare(lib_xbrick(1708),key=1708, & 
& nodecnc=[1635,352,1458,1459,4207,2924,4030,4031,21841, 21842,21843, 21844,21790, 21789,12860, 12859 & 
& ,21845, 21846,21847, 21848,21792, 21791,12866, 12865], & 
& edgecnc=[8349,8350,8323,3858,8351,8352,8324,3861], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1708),elname="xbrick",eltype="xbrick",typekey=1708) 

        call prepare(lib_xbrick(1709),key=1709, & 
& nodecnc=[1475,1460,475,474,4047,4032,3047,3046,9536, 9535,9548, 9547,21849, 21850,13902, 13901,9544 & 
& , 9543,9556, 9555,21851, 21852,13908, 13907], & 
& edgecnc=[2196,2202,8353,4379,2200,2206,8354,4382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1709),elname="xbrick",eltype="xbrick",typekey=1709) 

        call prepare(lib_xbrick(1710),key=1710, & 
& nodecnc=[350,2222,1498,1478,2922,4794,4070,4050,21830, 21829,21853, 21854,13018, 13017,21855, 21856 & 
& ,21834, 21833,21857, 21858,13026, 13025,21859, 21860], & 
& edgecnc=[8343,8355,3937,8356,8345,8357,3941,8358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1710),elname="xbrick",eltype="xbrick",typekey=1710) 

        call prepare(lib_xbrick(1711),key=1711, & 
& nodecnc=[1659,346,1633,1775,4231,2918,4205,4347,21861, 21862,20662, 20661,21863, 21864,5574, 5573,21865 & 
& , 21866,20664, 20663,21867, 21868,5582, 5581], & 
& edgecnc=[8359,7759,8360,215,8361,7760,8362,219], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1711),elname="xbrick",eltype="xbrick",typekey=1711) 

        call prepare(lib_xbrick(1712),key=1712, & 
& nodecnc=[1482,398,397,1465,4054,2970,2969,4037,21869, 21870,21871, 21872,6590, 6589,18826, 18825,21873 & 
& , 21874,21875, 21876,6598, 6597,18832, 18831], & 
& edgecnc=[8363,8364,723,6841,8365,8366,727,6844], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1712),elname="xbrick",eltype="xbrick",typekey=1712) 

        call prepare(lib_xbrick(1713),key=1713, & 
& nodecnc=[1466,37,398,1482,4038,2609,2970,4054,6650, 6649,21877, 21878,21870, 21869,13302, 13301,6658 & 
& , 6657,21879, 21880,21874, 21873,13310, 13309], & 
& edgecnc=[753,8367,8363,4079,757,8368,8365,4083], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1713),elname="xbrick",eltype="xbrick",typekey=1713) 

        call prepare(lib_xbrick(1714),key=1714, & 
& nodecnc=[1464,1708,241,1465,4036,4280,2813,4037,6518, 6517,19018, 19017,18828, 18827,6596, 6595,6526 & 
& , 6525,19024, 19023,18834, 18833,6604, 6603], & 
& edgecnc=[687,6937,6842,726,691,6940,6845,730], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1714),elname="xbrick",eltype="xbrick",typekey=1714) 

        call prepare(lib_xbrick(1715),key=1715, & 
& nodecnc=[231,1728,1468,1469,2803,4300,4040,4041,7332, 7331,13432, 13431,7374, 7373,21881, 21882,7336 & 
& , 7335,13436, 13435,7380, 7379,21883, 21884], & 
& edgecnc=[1094,4144,1115,8369,1096,4146,1118,8370], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1715),elname="xbrick",eltype="xbrick",typekey=1715) 

        call prepare(lib_xbrick(1716),key=1716, & 
& nodecnc=[1486,1675,231,1469,4058,4247,2803,4041,7356, 7355,7300, 7299,21882, 21881,13424, 13423,7364 & 
& , 7363,7308, 7307,21884, 21883,13428, 13427], & 
& edgecnc=[1106,1078,8369,4140,1110,1082,8370,4142], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1716),elname="xbrick",eltype="xbrick",typekey=1716) 

        call prepare(lib_xbrick(1717),key=1717, & 
& nodecnc=[232,1470,1485,1674,2804,4042,4057,4246,7360, 7359,13414, 13413,7384, 7383,21885, 21886,7368 & 
& , 7367,13420, 13419,7392, 7391,21887, 21888], & 
& edgecnc=[1108,4135,1120,8371,1112,4138,1124,8372], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1717),elname="xbrick",eltype="xbrick",typekey=1717) 

        call prepare(lib_xbrick(1718),key=1718, & 
& nodecnc=[1471,1493,458,457,4043,4065,3030,3029,21889, 21890,21891, 21892,21893, 21894,8252, 8251,21895 & 
& , 21896,21897, 21898,21899, 21900,8260, 8259], & 
& edgecnc=[8373,8374,8375,1554,8376,8377,8378,1558], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1718),elname="xbrick",eltype="xbrick",typekey=1718) 

        call prepare(lib_xbrick(1719),key=1719, & 
& nodecnc=[1802,1811,339,2259,4374,4383,2911,4831,21901, 21902,21903, 21904,19812, 19811,19828, 19827 & 
& ,21905, 21906,21907, 21908,19816, 19815,19832, 19831], & 
& edgecnc=[8379,8380,7334,7342,8381,8382,7336,7344], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1719),elname="xbrick",eltype="xbrick",typekey=1719) 

        call prepare(lib_xbrick(1720),key=1720, & 
& nodecnc=[1186,400,399,1472,3758,2972,2971,4044,20054, 20053,21909, 21910,6654, 6653,13318, 13317,20058 & 
& , 20057,21911, 21912,6662, 6661,13322, 13321], & 
& edgecnc=[7455,8383,755,4087,7457,8384,759,4089], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1720),elname="xbrick",eltype="xbrick",typekey=1720) 

        call prepare(lib_xbrick(1721),key=1721, & 
& nodecnc=[1493,1473,70,458,4065,4045,2642,3030,13756, 13755,19820, 19819,21913, 21914,21892, 21891,13762 & 
& , 13761,19824, 19823,21915, 21916,21898, 21897], & 
& edgecnc=[4306,7338,8385,8374,4309,7340,8386,8377], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1721),elname="xbrick",eltype="xbrick",typekey=1721) 

        call prepare(lib_xbrick(1722),key=1722, & 
& nodecnc=[1460,1458,352,1503,4032,4030,2924,4075,13912, 13911,21844, 21843,13924, 13923,9550, 9549,13918 & 
& , 13917,21848, 21847,13930, 13929,9558, 9557], & 
& edgecnc=[4384,8350,4390,2203,4387,8352,4393,2207], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1722),elname="xbrick",eltype="xbrick",typekey=1722) 

        call prepare(lib_xbrick(1723),key=1723, & 
& nodecnc=[1476,1483,42,408,4048,4055,2614,2980,12222, 12221,12236, 12235,21917, 21918,11628, 11627,12228 & 
& , 12227,12244, 12243,21919, 21920,11636, 11635], & 
& edgecnc=[3539,3546,8387,3242,3542,3550,8388,3246], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1723),elname="xbrick",eltype="xbrick",typekey=1723) 

        call prepare(lib_xbrick(1724),key=1724, & 
& nodecnc=[2230,1625,1667,234,4802,4197,4239,2806,21808, 21807,21921, 21922,13390, 13389,21923, 21924 & 
& ,21812, 21811,21925, 21926,13394, 13393,21927, 21928], & 
& edgecnc=[8332,8389,4123,8390,8334,8391,4125,8392], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1724),elname="xbrick",eltype="xbrick",typekey=1724) 

        call prepare(lib_xbrick(1725),key=1725, & 
& nodecnc=[1479,1595,376,25,4051,4167,2948,2597,12440, 12439,12424, 12423,21929, 21930,13180, 13179,12446 & 
& , 12445,12432, 12431,21931, 21932,13188, 13187], & 
& edgecnc=[3648,3640,8393,4018,3651,3644,8394,4022], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1725),elname="xbrick",eltype="xbrick",typekey=1725) 

        call prepare(lib_xbrick(1726),key=1726, & 
& nodecnc=[256,1714,1480,1595,2828,4286,4052,4167,6062, 6061,13090, 13089,12426, 12425,12438, 12437,6068 & 
& , 6067,13096, 13095,12434, 12433,12444, 12443], & 
& edgecnc=[459,3973,3641,3647,462,3976,3645,3650], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1726),elname="xbrick",eltype="xbrick",typekey=1726) 

        call prepare(lib_xbrick(1727),key=1727, & 
& nodecnc=[242,1611,1501,1218,2814,4183,4073,3790,19662, 19661,6522, 6521,20282, 20281,19950, 19949,19664 & 
& , 19663,6530, 6529,20286, 20285,19952, 19951], & 
& edgecnc=[7259,689,7569,7403,7260,693,7571,7404], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1727),elname="xbrick",eltype="xbrick",typekey=1727) 

        call prepare(lib_xbrick(1728),key=1728, & 
& nodecnc=[43,410,1484,1485,2615,2982,4056,4057,21933, 21934,13402, 13401,7386, 7385,13412, 13411,21935 & 
& , 21936,13408, 13407,7394, 7393,13418, 13417], & 
& edgecnc=[8395,4129,1121,4134,8396,4132,1125,4137], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1728),elname="xbrick",eltype="xbrick",typekey=1728) 

        call prepare(lib_xbrick(1729),key=1729, & 
& nodecnc=[1477,1667,233,1484,4049,4239,2805,4056,13392, 13391,21937, 21938,7388, 7387,13400, 13399,13396 & 
& , 13395,21939, 21940,7396, 7395,13406, 13405], & 
& edgecnc=[4124,8397,1122,4128,4126,8398,1126,4131], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1729),elname="xbrick",eltype="xbrick",typekey=1729) 

        call prepare(lib_xbrick(1730),key=1730, & 
& nodecnc=[230,1718,1467,1487,2802,4290,4039,4059,21941, 21942,7276, 7275,13440, 13439,13430, 13429,21943 & 
& , 21944,7280, 7279,13444, 13443,13434, 13433], & 
& edgecnc=[8399,1066,4148,4143,8400,1068,4150,4145], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1730),elname="xbrick",eltype="xbrick",typekey=1730) 

        call prepare(lib_xbrick(1731),key=1731, & 
& nodecnc=[1488,1800,229,1489,4060,4372,2801,4061,7274, 7273,21945, 21946,6824, 6823,13448, 13447,7278 & 
& , 7277,21947, 21948,6832, 6831,13452, 13451], & 
& edgecnc=[1065,8401,840,4152,1067,8402,844,4154], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1731),elname="xbrick",eltype="xbrick",typekey=1731) 

        call prepare(lib_xbrick(1732),key=1732, & 
& nodecnc=[1490,1588,438,59,4062,4160,3010,2631,7552, 7551,6960, 6959,21949, 21950,13620, 13619,7558, 7557 & 
& ,6968, 6967,21951, 21952,13626, 13625], & 
& edgecnc=[1204,908,8403,4238,1207,912,8404,4241], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1732),elname="xbrick",eltype="xbrick",typekey=1732) 

        call prepare(lib_xbrick(1733),key=1733, & 
& nodecnc=[326,1713,1491,1588,2898,4285,4063,4160,6946, 6945,6978, 6977,6962, 6961,7550, 7549,6954, 6953 & 
& ,6986, 6985,6970, 6969,7556, 7555], & 
& edgecnc=[901,917,909,1203,905,921,913,1206], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1733),elname="xbrick",eltype="xbrick",typekey=1733) 

        call prepare(lib_xbrick(1734),key=1734, & 
& nodecnc=[1257,1302,346,1659,3829,3874,2918,4231,9730, 9729,20426, 20425,21862, 21861,5586, 5585,9738 & 
& , 9737,20432, 20431,21866, 21865,5592, 5591], & 
& edgecnc=[2293,7641,8359,221,2297,7644,8361,224], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1734),elname="xbrick",eltype="xbrick",typekey=1734) 

        call prepare(lib_xbrick(1735),key=1735, & 
& nodecnc=[2208,1496,1385,1369,4780,4068,3957,3941,13862, 13861,21444, 21443,13852, 13851,8882, 8881,13868 & 
& , 13867,21448, 21447,13858, 13857,8890, 8889], & 
& edgecnc=[4359,8150,4354,1869,4362,8152,4357,1873], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1735),elname="xbrick",eltype="xbrick",typekey=1735) 

        call prepare(lib_xbrick(1736),key=1736, & 
& nodecnc=[1668,1497,1640,349,4240,4069,4212,2921,21953, 21954,13878, 13877,21458, 21457,21222, 21221 & 
& ,21955, 21956,13886, 13885,21460, 21459,21226, 21225], & 
& edgecnc=[8405,4367,8157,8039,8406,4371,8158,8041], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1736),elname="xbrick",eltype="xbrick",typekey=1736) 

        call prepare(lib_xbrick(1737),key=1737, & 
& nodecnc=[2222,1461,1475,1498,4794,4033,4047,4070,21957, 21958,9530, 9529,13900, 13899,21854, 21853,21959 & 
& , 21960,9538, 9537,13906, 13905,21858, 21857], & 
& edgecnc=[8407,2193,4378,8355,8408,2197,4381,8357], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1737),elname="xbrick",eltype="xbrick",typekey=1737) 

        call prepare(lib_xbrick(1738),key=1738, & 
& nodecnc=[1499,1801,353,1500,4071,4373,2925,4072,13058, 13057,21961, 21962,9426, 9425,13936, 13935,13064 & 
& , 13063,21963, 21964,9432, 9431,13940, 13939], & 
& edgecnc=[3957,8409,2141,4396,3960,8410,2144,4398], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1738),elname="xbrick",eltype="xbrick",typekey=1738) 

        call prepare(lib_xbrick(1739),key=1739, & 
& nodecnc=[1494,467,74,1502,4066,3039,2646,4074,13834, 13833,21965, 21966,20478, 20477,5588, 5587,13840 & 
& , 13839,21967, 21968,20482, 20481,5594, 5593], & 
& edgecnc=[4345,8411,7667,222,4348,8412,7669,225], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1739),elname="xbrick",eltype="xbrick",typekey=1739) 

        call prepare(lib_xbrick(1740),key=1740, & 
& nodecnc=[1512,357,1679,1504,4084,2929,4251,4076,21969, 21970,13046, 13045,5922, 5921,14002, 14001,21971 & 
& , 21972,13052, 13051,5928, 5927,14006, 14005], & 
& edgecnc=[8413,3951,389,4429,8414,3954,392,4431], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1740),elname="xbrick",eltype="xbrick",typekey=1740) 

        call prepare(lib_xbrick(1741),key=1741, & 
& nodecnc=[1509,426,52,1505,4081,2998,2624,4077,7662, 7661,21973, 21974,13522, 13521,7678, 7677,7670, 7669 & 
& ,21975, 21976,13526, 13525,7684, 7683], & 
& edgecnc=[1259,8415,4189,1267,1263,8416,4191,1270], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1741),elname="xbrick",eltype="xbrick",typekey=1741) 

        call prepare(lib_xbrick(1742),key=1742, & 
& nodecnc=[91,1510,1506,1677,2663,4082,4078,4249,7730, 7729,13534, 13533,13530, 13529,7700, 7699,7736 & 
& , 7735,13538, 13537,13532, 13531,7706, 7705], & 
& edgecnc=[1293,4195,4193,1278,1296,4197,4194,1281], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1742),elname="xbrick",eltype="xbrick",typekey=1742) 

        call prepare(lib_xbrick(1743),key=1743, & 
& nodecnc=[156,1514,1507,1678,2728,4086,4079,4250,9582, 9581,14014, 14013,14010, 14009,11460, 11459,9588 & 
& , 9587,14018, 14017,14012, 14011,11466, 11465], & 
& edgecnc=[2219,4435,4433,3158,2222,4437,4434,3161], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1743),elname="xbrick",eltype="xbrick",typekey=1743) 

        call prepare(lib_xbrick(1744),key=1744, & 
& nodecnc=[1560,1602,224,1508,4132,4174,2796,4080,13502, 13501,21977, 21978,7594, 7593,13510, 13509,13506 & 
& , 13505,21979, 21980,7602, 7601,13516, 13515], & 
& edgecnc=[4179,8417,1225,4183,4181,8418,1229,4186], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1744),elname="xbrick",eltype="xbrick",typekey=1744) 

        call prepare(lib_xbrick(1745),key=1745, & 
& nodecnc=[93,1530,1511,1680,2665,4102,4083,4252,7766, 7765,7738, 7737,7728, 7727,21981, 21982,7774, 7773 & 
& ,7744, 7743,7734, 7733,21983, 21984], & 
& edgecnc=[1311,1297,1292,8419,1315,1300,1295,8420], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1745),elname="xbrick",eltype="xbrick",typekey=1745) 

        call prepare(lib_xbrick(1746),key=1746, & 
& nodecnc=[1580,1609,357,1512,4152,4181,2929,4084,13982, 13981,21985, 21986,21970, 21969,13990, 13989 & 
& ,13986, 13985,21987, 21988,21972, 21971,13996, 13995], & 
& edgecnc=[4419,8421,8413,4423,4421,8422,8414,4426], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1746),elname="xbrick",eltype="xbrick",typekey=1746) 

        call prepare(lib_xbrick(1747),key=1747, & 
& nodecnc=[92,1542,1515,1681,2664,4114,4087,4253,9634, 9633,9622, 9621,9580, 9579,10178, 10177,9642, 9641 & 
& ,9628, 9627,9586, 9585,10186, 10185], & 
& edgecnc=[2245,2239,2218,2517,2249,2242,2221,2521], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1747),elname="xbrick",eltype="xbrick",typekey=1747) 

        call prepare(lib_xbrick(1748),key=1748, & 
& nodecnc=[1722,1516,1525,248,4294,4088,4097,2820,13256, 13255,6310, 6309,13246, 13245,8958, 8957,13260 & 
& , 13259,6316, 6315,13250, 13249,8966, 8965], & 
& edgecnc=[4056,583,4051,1907,4058,586,4053,1911], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1748),elname="xbrick",eltype="xbrick",typekey=1748) 

        call prepare(lib_xbrick(1749),key=1749, & 
& nodecnc=[1517,1551,29,383,4089,4123,2601,2955,21989, 21990,6018, 6017,21991, 21992,5984, 5983,21993 & 
& , 21994,6024, 6023,21995, 21996,5992, 5991], & 
& edgecnc=[8423,437,8424,420,8425,440,8426,424], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1749),elname="xbrick",eltype="xbrick",typekey=1749) 

        call prepare(lib_xbrick(1750),key=1750, & 
& nodecnc=[251,1551,1517,1772,2823,4123,4089,4344,13240, 13239,21990, 21989,13230, 13229,12398, 12397 & 
& ,13244, 13243,21994, 21993,13234, 13233,12404, 12403], & 
& edgecnc=[4048,8423,4043,3627,4050,8425,4045,3630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1750),elname="xbrick",eltype="xbrick",typekey=1750) 

        call prepare(lib_xbrick(1751),key=1751, & 
& nodecnc=[250,1552,1518,1726,2822,4124,4090,4298,5754, 5753,5998, 5997,13238, 13237,10268, 10267,5762 & 
& , 5761,6006, 6005,13242, 13241,10274, 10273], & 
& edgecnc=[305,427,4047,2562,309,431,4049,2565], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1751),elname="xbrick",eltype="xbrick",typekey=1751) 

        call prepare(lib_xbrick(1752),key=1752, & 
& nodecnc=[2206,497,496,1519,4778,3069,3068,4091,6030, 6029,21997, 21998,12452, 12451,13166, 13165,6038 & 
& , 6037,21999, 22000,12458, 12457,13174, 13173], & 
& edgecnc=[443,8427,3654,4011,447,8428,3657,4015], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1752),elname="xbrick",eltype="xbrick",typekey=1752) 

        call prepare(lib_xbrick(1753),key=1753, & 
& nodecnc=[191,1689,1520,1584,2763,4261,4092,4156,6158, 6157,6208, 6207,14046, 14045,6174, 6173,6166, 6165 & 
& ,6216, 6215,14054, 14053,6182, 6181], & 
& edgecnc=[507,532,4451,515,511,536,4455,519], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1753),elname="xbrick",eltype="xbrick",typekey=1753) 

        call prepare(lib_xbrick(1754),key=1754, & 
& nodecnc=[1585,495,89,1520,4157,3067,2661,4092,6190, 6189,22001, 22002,14048, 14047,6206, 6205,6198, 6197 & 
& ,22003, 22004,14056, 14055,6214, 6213], & 
& edgecnc=[523,8429,4452,531,527,8430,4456,535], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1754),elname="xbrick",eltype="xbrick",typekey=1754) 

        call prepare(lib_xbrick(1755),key=1755, & 
& nodecnc=[1672,1911,1548,1547,4244,4483,4120,4119,22005, 22006,22007, 22008,22009, 22010,6276, 6275,22011 & 
& , 22012,22013, 22014,22015, 22016,6284, 6283], & 
& edgecnc=[8431,8432,8433,566,8434,8435,8436,570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1755),elname="xbrick",eltype="xbrick",typekey=1755) 

        call prepare(lib_xbrick(1756),key=1756, & 
& nodecnc=[2219,1523,1481,255,4791,4095,4053,2827,6280, 6279,6262, 6261,13086, 13085,22017, 22018,6288 & 
& , 6287,6270, 6269,13092, 13091,22019, 22020], & 
& edgecnc=[568,559,3971,8437,572,563,3974,8438], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1756),elname="xbrick",eltype="xbrick",typekey=1756) 

        call prepare(lib_xbrick(1757),key=1757, & 
& nodecnc=[1524,1554,31,386,4096,4126,2603,2958,5736, 5735,22021, 22022,22023, 22024,6292, 6291,5744, 5743 & 
& ,22025, 22026,22027, 22028,6300, 6299], & 
& edgecnc=[296,8439,8440,574,300,8441,8442,578], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1757),elname="xbrick",eltype="xbrick",typekey=1757) 

        call prepare(lib_xbrick(1758),key=1758, & 
& nodecnc=[249,1524,1553,1692,2821,4096,4125,4264,5738, 5737,6290, 6289,5750, 5749,10288, 10287,5746, 5745 & 
& ,6298, 6297,5758, 5757,10294, 10293], & 
& edgecnc=[297,573,303,2572,301,577,307,2575], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1758),elname="xbrick",eltype="xbrick",typekey=1758) 

        call prepare(lib_xbrick(1759),key=1759, & 
& nodecnc=[1554,1525,387,31,4126,4097,2959,2603,13248, 13247,6308, 6307,22029, 22030,22022, 22021,13252 & 
& , 13251,6314, 6313,22031, 22032,22026, 22025], & 
& edgecnc=[4052,582,8443,8439,4054,585,8444,8441], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1759),elname="xbrick",eltype="xbrick",typekey=1759) 

        call prepare(lib_xbrick(1760),key=1760, & 
& nodecnc=[1555,1526,389,32,4127,4098,2961,2604,13262, 13261,6324, 6323,22033, 22034,5720, 5719,13264 & 
& , 13263,6332, 6331,22035, 22036,5728, 5727], & 
& edgecnc=[4059,590,8445,288,4060,594,8446,292], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1760),elname="xbrick",eltype="xbrick",typekey=1760) 

        call prepare(lib_xbrick(1761),key=1761, & 
& nodecnc=[246,1997,1527,1557,2818,4569,4099,4129,22037, 22038,22039, 22040,9962, 9961,22041, 22042,22043 & 
& , 22044,22045, 22046,9968, 9967,22047, 22048], & 
& edgecnc=[8447,8448,2409,8449,8450,8451,2412,8452], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1761),elname="xbrick",eltype="xbrick",typekey=1761) 

        call prepare(lib_xbrick(1762),key=1762, & 
& nodecnc=[1556,1799,246,1557,4128,4371,2818,4129,6334, 6333,22049, 22050,22042, 22041,22051, 22052,6340 & 
& , 6339,22053, 22054,22048, 22047,22055, 22056], & 
& edgecnc=[595,8453,8449,8454,598,8455,8452,8456], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1762),elname="xbrick",eltype="xbrick",typekey=1762) 

        call prepare(lib_xbrick(1763),key=1763, & 
& nodecnc=[2218,1529,1492,327,4790,4101,4064,2899,7012, 7011,6994, 6993,6974, 6973,22057, 22058,7020, 7019 & 
& ,7002, 7001,6982, 6981,22059, 22060], & 
& edgecnc=[934,925,915,8457,938,929,919,8458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1763),elname="xbrick",eltype="xbrick",typekey=1763) 

        call prepare(lib_xbrick(1764),key=1764, & 
& nodecnc=[1562,430,429,1561,4134,3002,3001,4133,13550, 13549,22061, 22062,13542, 13541,22063, 22064,13556 & 
& , 13555,22065, 22066,13546, 13545,22067, 22068], & 
& edgecnc=[4203,8459,4199,8460,4206,8461,4201,8462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1764),elname="xbrick",eltype="xbrick",typekey=1764) 

        call prepare(lib_xbrick(1765),key=1765, & 
& nodecnc=[1564,432,431,1563,4136,3004,3003,4135,13564, 13563,22069, 22070,7750, 7749,7788, 7787,13570 & 
& , 13569,22071, 22072,7758, 7757,7796, 7795], & 
& edgecnc=[4210,8463,1303,1322,4213,8464,1307,1326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1765),elname="xbrick",eltype="xbrick",typekey=1765) 

        call prepare(lib_xbrick(1766),key=1766, & 
& nodecnc=[2220,1563,1531,1686,4792,4135,4103,4258,7782, 7781,7756, 7755,22073, 22074,22075, 22076,7790 & 
& , 7789,7764, 7763,22077, 22078,22079, 22080], & 
& edgecnc=[1319,1306,8465,8466,1323,1310,8467,8468], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1766),elname="xbrick",eltype="xbrick",typekey=1766) 

        call prepare(lib_xbrick(1767),key=1767, & 
& nodecnc=[1565,433,56,1532,4137,3005,2628,4104,22081, 22082,22083, 22084,13566, 13565,7826, 7825,22085 & 
& , 22086,22087, 22088,13572, 13571,7828, 7827], & 
& edgecnc=[8469,8470,4211,1341,8471,8472,4214,1342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1767),elname="xbrick",eltype="xbrick",typekey=1767) 

        call prepare(lib_xbrick(1768),key=1768, & 
& nodecnc=[1533,57,433,1565,4105,2629,3005,4137,13586, 13585,22089, 22090,22082, 22081,7814, 7813,13592 & 
& , 13591,22091, 22092,22086, 22085,7822, 7821], & 
& edgecnc=[4221,8473,8469,1335,4224,8474,8471,1339], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1768),elname="xbrick",eltype="xbrick",typekey=1768) 

        call prepare(lib_xbrick(1769),key=1769, & 
& nodecnc=[2205,435,434,1566,4777,3007,3006,4138,13598, 13597,22093, 22094,13588, 13587,13612, 13611,13604 & 
& , 13603,22095, 22096,13594, 13593,13616, 13615], & 
& edgecnc=[4227,8475,4222,4234,4230,8476,4225,4236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1769),elname="xbrick",eltype="xbrick",typekey=1769) 

        call prepare(lib_xbrick(1770),key=1770, & 
& nodecnc=[1534,1571,63,445,4106,4143,2635,3017,22097, 22098,8092, 8091,22099, 22100,8040, 8039,22101 & 
& , 22102,8100, 8099,22103, 22104,8048, 8047], & 
& edgecnc=[8477,1474,8478,1448,8479,1478,8480,1452], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1770),elname="xbrick",eltype="xbrick",typekey=1770) 

        call prepare(lib_xbrick(1771),key=1771, & 
& nodecnc=[331,1571,1534,1773,2903,4143,4106,4345,13674, 13673,22098, 22097,13666, 13665,10650, 10649 & 
& ,13676, 13675,22102, 22101,13670, 13669,10654, 10653], & 
& edgecnc=[4265,8477,4261,2753,4266,8479,4263,2755], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1771),elname="xbrick",eltype="xbrick",typekey=1771) 

        call prepare(lib_xbrick(1772),key=1772, & 
& nodecnc=[329,1569,1535,2098,2901,4141,4107,4670,8060, 8059,8070, 8069,22105, 22106,22107, 22108,8068 & 
& , 8067,8078, 8077,22109, 22110,22111, 22112], & 
& edgecnc=[1458,1463,8481,8482,1462,1467,8483,8484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1772),elname="xbrick",eltype="xbrick",typekey=1772) 

        call prepare(lib_xbrick(1773),key=1773, & 
& nodecnc=[1537,1574,65,448,4109,4146,2637,3020,12950, 12949,22113, 22114,22115, 22116,13690, 13689,12952 & 
& , 12951,22117, 22118,22119, 22120,13696, 13695], & 
& edgecnc=[3903,8485,8486,4273,3904,8487,8488,4276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1773),elname="xbrick",eltype="xbrick",typekey=1773) 

        call prepare(lib_xbrick(1774),key=1774, & 
& nodecnc=[1574,1538,449,65,4146,4110,3021,2637,8140, 8139,8164, 8163,22121, 22122,22114, 22113,8148, 8147 & 
& ,8172, 8171,22123, 22124,22118, 22117], & 
& edgecnc=[1498,1510,8489,8485,1502,1514,8490,8487], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1774),elname="xbrick",eltype="xbrick",typekey=1774) 

        call prepare(lib_xbrick(1775),key=1775, & 
& nodecnc=[66,450,1540,1575,2638,3022,4112,4147,22125, 22126,8168, 8167,13700, 13699,13712, 13711,22127 & 
& , 22128,8176, 8175,13706, 13705,13718, 13717], & 
& edgecnc=[8491,1512,4278,4284,8492,1516,4281,4287], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1775),elname="xbrick",eltype="xbrick",typekey=1775) 

        call prepare(lib_xbrick(1776),key=1776, & 
& nodecnc=[1723,1540,1538,334,4295,4112,4110,2906,13702, 13701,8166, 8165,8138, 8137,8182, 8181,13708 & 
& , 13707,8174, 8173,8146, 8145,8190, 8189], & 
& edgecnc=[4279,1511,1497,1519,4282,1515,1501,1523], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1776),elname="xbrick",eltype="xbrick",typekey=1776) 

        call prepare(lib_xbrick(1777),key=1777, & 
& nodecnc=[336,1730,1541,1577,2908,4302,4113,4149,7154, 7153,21654, 21653,13726, 13725,8208, 8207,7162 & 
& , 7161,21658, 21657,13732, 13731,8214, 8213], & 
& edgecnc=[1005,8255,4291,1532,1009,8257,4294,1535], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1777),elname="xbrick",eltype="xbrick",typekey=1777) 

        call prepare(lib_xbrick(1778),key=1778, & 
& nodecnc=[1582,492,491,1581,4154,3064,3063,4153,14030, 14029,22129, 22130,14022, 14021,22131, 22132,14036 & 
& , 14035,22133, 22134,14026, 14025,22135, 22136], & 
& edgecnc=[4443,8493,4439,8494,4446,8495,4441,8496], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1778),elname="xbrick",eltype="xbrick",typekey=1778) 

        call prepare(lib_xbrick(1779),key=1779, & 
& nodecnc=[1584,494,493,1583,4156,3066,3065,4155,14044, 14043,22137, 22138,9674, 9673,6176, 6175,14052 & 
& , 14051,22139, 22140,9682, 9681,6184, 6183], & 
& edgecnc=[4450,8497,2265,516,4454,8498,2269,520], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1779),elname="xbrick",eltype="xbrick",typekey=1779) 

        call prepare(lib_xbrick(1780),key=1780, & 
& nodecnc=[244,1544,1559,1774,2816,4116,4131,4346,13282, 13281,13270, 13269,22141, 22142,22143, 22144 & 
& ,13288, 13287,13276, 13275,22145, 22146,22147, 22148], & 
& edgecnc=[4069,4063,8499,8500,4072,4066,8501,8502], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1780),elname="xbrick",eltype="xbrick",typekey=1780) 

        call prepare(lib_xbrick(1781),key=1781, & 
& nodecnc=[291,1685,1545,1585,2863,4257,4117,4157,22149, 22150,14058, 14057,6192, 6191,6204, 6203,22151 & 
& , 22152,14062, 14061,6200, 6199,6212, 6211], & 
& edgecnc=[8503,4457,524,530,8504,4459,528,534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1781),elname="xbrick",eltype="xbrick",typekey=1781) 

        call prepare(lib_xbrick(1782),key=1782, & 
& nodecnc=[27,379,1547,1548,2599,2951,4119,4120,22153, 22154,13200, 13199,22010, 22009,13208, 13207,22155 & 
& , 22156,13204, 13203,22016, 22015,13214, 13213], & 
& edgecnc=[8505,4028,8433,4032,8506,4030,8436,4035], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1782),elname="xbrick",eltype="xbrick",typekey=1782) 

        call prepare(lib_xbrick(1783),key=1783, & 
& nodecnc=[2101,1521,1548,1911,4673,4093,4120,4483,6222, 6221,13210, 13209,22008, 22007,22157, 22158,6230 & 
& , 6229,13216, 13215,22014, 22013,22159, 22160], & 
& edgecnc=[539,4033,8432,8507,543,4036,8435,8508], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1783),elname="xbrick",eltype="xbrick",typekey=1783) 

        call prepare(lib_xbrick(1784),key=1784, & 
& nodecnc=[28,381,1549,1550,2600,2953,4121,4122,22161, 22162,6234, 6233,6250, 6249,13220, 13219,22163 & 
& , 22164,6240, 6239,6256, 6255,13226, 13225], & 
& edgecnc=[8509,545,553,4038,8510,548,556,4041], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1784),elname="xbrick",eltype="xbrick",typekey=1784) 

        call prepare(lib_xbrick(1785),key=1785, & 
& nodecnc=[2066,252,1522,1550,4638,2824,4094,4122,22165, 22166,13232, 13231,13222, 13221,6248, 6247,22167 & 
& , 22168,13236, 13235,13228, 13227,6254, 6253], & 
& edgecnc=[8511,4044,4039,552,8512,4046,4042,555], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1785),elname="xbrick",eltype="xbrick",typekey=1785) 

        call prepare(lib_xbrick(1786),key=1786, & 
& nodecnc=[30,385,1552,1553,2602,2957,4124,4125,22169, 22170,6000, 5999,5752, 5751,6296, 6295,22171, 22172 & 
& ,6008, 6007,5760, 5759,6304, 6303], & 
& edgecnc=[8513,428,304,576,8514,432,308,580], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1786),elname="xbrick",eltype="xbrick",typekey=1786) 

        call prepare(lib_xbrick(1787),key=1787, & 
& nodecnc=[33,390,1556,1557,2605,2962,4128,4129,22173, 22174,6320, 6319,22052, 22051,9966, 9965,22175 & 
& , 22176,6328, 6327,22056, 22055,9972, 9971], & 
& edgecnc=[8515,588,8454,2411,8516,592,8456,2414], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1787),elname="xbrick",eltype="xbrick",typekey=1787) 

        call prepare(lib_xbrick(1788),key=1788, & 
& nodecnc=[34,392,1558,1559,2606,2964,4130,4131,22177, 22178,6348, 6347,22179, 22180,13268, 13267,22181 & 
& , 22182,6356, 6355,22183, 22184,13274, 13273], & 
& edgecnc=[8517,602,8518,4062,8519,606,8520,4065], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1788),elname="xbrick",eltype="xbrick",typekey=1788) 

        call prepare(lib_xbrick(1789),key=1789, & 
& nodecnc=[1527,1997,245,1558,4099,4569,2817,4130,22040, 22039,22185, 22186,22187, 22188,6346, 6345,22046 & 
& , 22045,22189, 22190,22191, 22192,6354, 6353], & 
& edgecnc=[8448,8521,8522,601,8451,8523,8524,605], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1789),elname="xbrick",eltype="xbrick",typekey=1789) 

        call prepare(lib_xbrick(1790),key=1790, & 
& nodecnc=[1696,224,1602,1590,4268,2796,4174,4162,7626, 7625,21978, 21977,7564, 7563,22193, 22194,7632 & 
& , 7631,21980, 21979,7572, 7571,22195, 22196], & 
& edgecnc=[1241,8417,1210,8525,1244,8418,1214,8526], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1790),elname="xbrick",eltype="xbrick",typekey=1790) 

        call prepare(lib_xbrick(1791),key=1791, & 
& nodecnc=[2246,2245,1932,2108,4818,4817,4504,4680,22197, 22198,22199, 22200,22201, 22202,22203, 22204 & 
& ,22205, 22206,22207, 22208,22209, 22210,22211, 22212], & 
& edgecnc=[8527,8528,8529,8530,8531,8532,8533,8534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1791),elname="xbrick",eltype="xbrick",typekey=1791) 

        call prepare(lib_xbrick(1792),key=1792, & 
& nodecnc=[1686,1531,1562,1932,4258,4103,4134,4504,22074, 22073,13552, 13551,22213, 22214,22215, 22216 & 
& ,22078, 22077,13558, 13557,22217, 22218,22219, 22220], & 
& edgecnc=[8465,4204,8535,8536,8467,4207,8537,8538], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1792),elname="xbrick",eltype="xbrick",typekey=1792) 

        call prepare(lib_xbrick(1793),key=1793, & 
& nodecnc=[61,441,1567,1568,2633,3013,4139,4140,22221, 22222,13636, 13635,7834, 7833,13644, 13643,22223 & 
& , 22224,13640, 13639,7840, 7839,13650, 13649], & 
& edgecnc=[8539,4246,1345,4250,8540,4248,1348,4253], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1793),elname="xbrick",eltype="xbrick",typekey=1793) 

        call prepare(lib_xbrick(1794),key=1794, & 
& nodecnc=[2098,1535,1568,1912,4670,4107,4140,4484,22106, 22105,13646, 13645,7832, 7831,22225, 22226,22110 & 
& , 22109,13652, 13651,7838, 7837,22227, 22228], & 
& edgecnc=[8481,4251,1344,8541,8483,4254,1347,8542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1794),elname="xbrick",eltype="xbrick",typekey=1794) 

        call prepare(lib_xbrick(1795),key=1795, & 
& nodecnc=[62,443,1569,1570,2634,3015,4141,4142,22229, 22230,8072, 8071,8058, 8057,13656, 13655,22231 & 
& , 22232,8080, 8079,8066, 8065,13662, 13661], & 
& edgecnc=[8543,1464,1457,4256,8544,1468,1461,4259], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1795),elname="xbrick",eltype="xbrick",typekey=1795) 

        call prepare(lib_xbrick(1796),key=1796, & 
& nodecnc=[2065,330,1546,1570,4637,2902,4118,4142,22233, 22234,13668, 13667,13658, 13657,8056, 8055,22235 & 
& , 22236,13672, 13671,13664, 13663,8064, 8063], & 
& edgecnc=[8545,4262,4257,1456,8546,4264,4260,1460], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1796),elname="xbrick",eltype="xbrick",typekey=1796) 

        call prepare(lib_xbrick(1797),key=1797, & 
& nodecnc=[64,447,1572,1573,2636,3019,4144,4145,22237, 22238,13680, 13679,10730, 10729,13688, 13687,22239 & 
& , 22240,13684, 13683,10732, 10731,13694, 13693], & 
& edgecnc=[8547,4268,2793,4272,8548,4270,2794,4275], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1797),elname="xbrick",eltype="xbrick",typekey=1797) 

        call prepare(lib_xbrick(1798),key=1798, & 
& nodecnc=[1716,1539,1575,335,4288,4111,4147,2907,6718, 6717,13714, 13713,13698, 13697,22241, 22242,6726 & 
& , 6725,13720, 13719,13704, 13703,22243, 22244], & 
& edgecnc=[787,4285,4277,8549,791,4288,4280,8550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1798),elname="xbrick",eltype="xbrick",typekey=1798) 

        call prepare(lib_xbrick(1799),key=1799, & 
& nodecnc=[67,452,1576,1577,2639,3024,4148,4149,22245, 22246,8150, 8149,8210, 8209,13724, 13723,22247 & 
& , 22248,8156, 8155,8216, 8215,13730, 13729], & 
& edgecnc=[8551,1503,1533,4290,8552,1506,1536,4293], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1799),elname="xbrick",eltype="xbrick",typekey=1799) 

        call prepare(lib_xbrick(1800),key=1800, & 
& nodecnc=[1729,1579,69,456,4301,4151,2641,3028,8238, 8237,8256, 8255,22249, 22250,20540, 20539,8246, 8245 & 
& ,8264, 8263,22251, 22252,20546, 20545], & 
& edgecnc=[1547,1556,8553,7698,1551,1560,8554,7701], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1800),elname="xbrick",eltype="xbrick",typekey=1800) 

        call prepare(lib_xbrick(1801),key=1801, & 
& nodecnc=[1697,357,1609,1591,4269,2929,4181,4163,13048, 13047,21986, 21985,9052, 9051,22253, 22254,13054 & 
& , 13053,21988, 21987,9060, 9059,22255, 22256], & 
& edgecnc=[3952,8421,1954,8555,3955,8422,1958,8556], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1801),elname="xbrick",eltype="xbrick",typekey=1801) 

        call prepare(lib_xbrick(1802),key=1802, & 
& nodecnc=[1688,1933,2243,109,4260,4505,4815,2681,22257, 22258,22259, 22260,10194, 10193,22261, 22262 & 
& ,22263, 22264,22265, 22266,10198, 10197,22267, 22268], & 
& edgecnc=[8557,8558,2525,8559,8560,8561,2527,8562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1802),elname="xbrick",eltype="xbrick",typekey=1802) 

        call prepare(lib_xbrick(1803),key=1803, & 
& nodecnc=[1688,1543,1582,1933,4260,4115,4154,4505,13098, 13097,14032, 14031,22269, 22270,22258, 22257 & 
& ,13102, 13101,14038, 14037,22271, 22272,22264, 22263], & 
& edgecnc=[3977,4444,8563,8557,3979,4447,8564,8560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1803),elname="xbrick",eltype="xbrick",typekey=1803) 

        call prepare(lib_xbrick(1804),key=1804, & 
& nodecnc=[1597,1586,1682,228,4169,4158,4254,2800,13456, 13455,6820, 6819,7940, 7939,6852, 6851,13462 & 
& , 13461,6828, 6827,7948, 7947,6860, 6859], & 
& edgecnc=[4156,838,1398,854,4159,842,1402,858], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1804),elname="xbrick",eltype="xbrick",typekey=1804) 

        call prepare(lib_xbrick(1805),key=1805, & 
& nodecnc=[1589,48,419,1599,4161,2620,2991,4171,7416, 7415,22273, 22274,6864, 6863,13484, 13483,7424, 7423 & 
& ,22275, 22276,6872, 6871,13492, 13491], & 
& edgecnc=[1136,8565,860,4170,1140,8566,864,4174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1805),elname="xbrick",eltype="xbrick",typekey=1805) 

        call prepare(lib_xbrick(1806),key=1806, & 
& nodecnc=[227,1694,1599,1587,2799,4266,4171,4159,7142, 7141,13478, 13477,6862, 6861,13468, 13467,7150 & 
& , 7149,13486, 13485,6870, 6869,13474, 13473], & 
& edgecnc=[999,4167,859,4162,1003,4171,863,4165], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1806),elname="xbrick",eltype="xbrick",typekey=1806) 

        call prepare(lib_xbrick(1807),key=1807, & 
& nodecnc=[1598,1587,418,47,4170,4159,2990,2619,13470, 13469,6868, 6867,22277, 22278,22279, 22280,13476 & 
& , 13475,6876, 6875,22281, 22282,22283, 22284], & 
& edgecnc=[4163,862,8567,8568,4166,866,8569,8570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1807),elname="xbrick",eltype="xbrick",typekey=1807) 

        call prepare(lib_xbrick(1808),key=1808, & 
& nodecnc=[226,1711,1600,1589,2798,4283,4172,4161,12188, 12187,7642, 7641,7410, 7409,13482, 13481,12194 & 
& , 12193,7648, 7647,7418, 7417,13490, 13489], & 
& edgecnc=[3522,1249,1133,4169,3525,1252,1137,4173], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1808),elname="xbrick",eltype="xbrick",typekey=1808) 

        call prepare(lib_xbrick(1809),key=1809, & 
& nodecnc=[225,1696,1590,1601,2797,4268,4162,4173,22285, 22286,22194, 22193,13494, 13493,7638, 7637,22287 & 
& , 22288,22196, 22195,13498, 13497,7644, 7643], & 
& edgecnc=[8571,8525,4175,1247,8572,8526,4177,1250], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1809),elname="xbrick",eltype="xbrick",typekey=1809) 

        call prepare(lib_xbrick(1810),key=1810, & 
& nodecnc=[356,1697,1591,1608,2928,4269,4163,4180,5866, 5865,22254, 22253,13974, 13973,9038, 9037,5874 & 
& , 5873,22256, 22255,13978, 13977,9044, 9043], & 
& edgecnc=[361,8555,4415,1947,365,8556,4417,1950], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1810),elname="xbrick",eltype="xbrick",typekey=1810) 

        call prepare(lib_xbrick(1811),key=1811, & 
& nodecnc=[355,1712,1607,1592,2927,4284,4179,4164,13036, 13035,9042, 9041,9130, 9129,22289, 22290,13042 & 
& , 13041,9048, 9047,9138, 9137,22291, 22292], & 
& edgecnc=[3946,1949,1993,8573,3949,1952,1997,8574], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1811),elname="xbrick",eltype="xbrick",typekey=1811) 

        call prepare(lib_xbrick(1812),key=1812, & 
& nodecnc=[355,1592,1606,1695,2927,4164,4178,4267,22290, 22289,13966, 13965,22293, 22294,22295, 22296 & 
& ,22292, 22291,13970, 13969,22297, 22298,22299, 22300], & 
& edgecnc=[8573,4411,8575,8576,8574,4413,8577,8578], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1812),elname="xbrick",eltype="xbrick",typekey=1812) 

        call prepare(lib_xbrick(1813),key=1813, & 
& nodecnc=[1593,2134,1695,1606,4165,4706,4267,4178,22301, 22302,9170, 9169,22294, 22293,9158, 9157,22303 & 
& , 22304,9178, 9177,22298, 22297,9166, 9165], & 
& edgecnc=[8579,2013,8575,2007,8580,2017,8577,2011], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1813),elname="xbrick",eltype="xbrick",typekey=1813) 

        call prepare(lib_xbrick(1814),key=1814, & 
& nodecnc=[1604,1594,1683,354,4176,4166,4255,2926,13944, 13943,9424, 9423,22305, 22306,9372, 9371,13950 & 
& , 13949,9430, 9429,22307, 22308,9380, 9379], & 
& edgecnc=[4400,2140,8581,2114,4403,2143,8582,2118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1814),elname="xbrick",eltype="xbrick",typekey=1814) 

        call prepare(lib_xbrick(1815),key=1815, & 
& nodecnc=[257,1725,1479,1596,2829,4297,4051,4168,22309, 22310,12442, 12441,13178, 13177,13162, 13161 & 
& ,22311, 22312,12448, 12447,13186, 13185,13170, 13169], & 
& edgecnc=[8583,3649,4017,4009,8584,3652,4021,4013], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1815),elname="xbrick",eltype="xbrick",typekey=1815) 

        call prepare(lib_xbrick(1816),key=1816, & 
& nodecnc=[47,417,1597,1598,2619,2989,4169,4170,22313, 22314,13458, 13457,6850, 6849,22280, 22279,22315 & 
& , 22316,13464, 13463,6858, 6857,22284, 22283], & 
& edgecnc=[8585,4157,853,8568,8586,4160,857,8570], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1816),elname="xbrick",eltype="xbrick",typekey=1816) 

        call prepare(lib_xbrick(1817),key=1817, & 
& nodecnc=[325,1724,1490,1603,2897,4296,4062,4175,22317, 22318,7554, 7553,13618, 13617,13610, 13609,22319 & 
& , 22320,7560, 7559,13624, 13623,13614, 13613], & 
& edgecnc=[8587,1205,4237,4233,8588,1208,4240,4235], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1817),elname="xbrick",eltype="xbrick",typekey=1817) 

        call prepare(lib_xbrick(1818),key=1818, & 
& nodecnc=[81,479,1604,1605,2653,3051,4176,4177,22321, 22322,13946, 13945,9370, 9369,13956, 13955,22323 & 
& , 22324,13952, 13951,9378, 9377,13962, 13961], & 
& edgecnc=[8589,4401,2113,4406,8590,4404,2117,4409], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1818),elname="xbrick",eltype="xbrick",typekey=1818) 

        call prepare(lib_xbrick(1819),key=1819, & 
& nodecnc=[1593,1605,2111,2134,4165,4177,4683,4706,13958, 13957,9368, 9367,22325, 22326,22302, 22301,13964 & 
& , 13963,9376, 9375,22327, 22328,22304, 22303], & 
& edgecnc=[4407,2112,8591,8579,4410,2116,8592,8580], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1819),elname="xbrick",eltype="xbrick",typekey=1819) 

        call prepare(lib_xbrick(1820),key=1820, & 
& nodecnc=[1669,2260,1461,2222,4241,4832,4033,4794,22329, 22330,12818, 12817,21958, 21957,21832, 21831 & 
& ,22331, 22332,12824, 12823,21960, 21959,21836, 21835], & 
& edgecnc=[8593,3837,8407,8344,8594,3840,8408,8346], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1820),elname="xbrick",eltype="xbrick",typekey=1820) 

        call prepare(lib_xbrick(1821),key=1821, & 
& nodecnc=[1456,1451,1613,2288,4028,4023,4185,4860,21736, 21735,21794, 21793,22333, 22334,22335, 22336 & 
& ,21742, 21741,21796, 21795,22337, 22338,22339, 22340], & 
& edgecnc=[8296,8325,8595,8596,8299,8326,8597,8598], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1821),elname="xbrick",eltype="xbrick",typekey=1821) 

        call prepare(lib_xbrick(1822),key=1822, & 
& nodecnc=[1750,1639,203,1670,4322,4211,2775,4242,20452, 20451,20394, 20393,22341, 22342,22343, 22344 & 
& ,20456, 20455,20396, 20395,22345, 22346,22347, 22348], & 
& edgecnc=[7654,7625,8599,8600,7656,7626,8601,8602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1822),elname="xbrick",eltype="xbrick",typekey=1822) 

        call prepare(lib_xbrick(1823),key=1823, & 
& nodecnc=[1794,279,1615,1720,4366,2851,4187,4292,22349, 22350,22351, 22352,9810, 9809,12522, 12521,22353 & 
& , 22354,22355, 22356,9818, 9817,12528, 12527], & 
& edgecnc=[8603,8604,2333,3689,8605,8606,2337,3692], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1823),elname="xbrick",eltype="xbrick",typekey=1823) 

        call prepare(lib_xbrick(1824),key=1824, & 
& nodecnc=[404,403,1382,1617,2976,2975,3954,4189,22357, 22358,21406, 21405,21294, 21293,11012, 11011,22359 & 
& , 22360,21412, 21411,21302, 21301,11020, 11019], & 
& edgecnc=[8607,8131,8075,2934,8608,8134,8079,2938], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1824),elname="xbrick",eltype="xbrick",typekey=1824) 

        call prepare(lib_xbrick(1825),key=1825, & 
& nodecnc=[1032,1067,1656,2214,3604,3639,4228,4786,17904, 17903,19166, 19165,22361, 22362,18970, 18969 & 
& ,17910, 17909,19172, 19171,22363, 22364,18972, 18971], & 
& edgecnc=[6380,7011,8609,6913,6383,7014,8610,6914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1825),elname="xbrick",eltype="xbrick",typekey=1825) 

        call prepare(lib_xbrick(1826),key=1826, & 
& nodecnc=[1619,2253,313,1293,4191,4825,2885,3865,5630, 5629,20644, 20643,20576, 20575,21142, 21141,5638 & 
& , 5637,20648, 20647,20580, 20579,21144, 21143], & 
& edgecnc=[243,7750,7716,7999,247,7752,7718,8000], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1826),elname="xbrick",eltype="xbrick",typekey=1826) 

        call prepare(lib_xbrick(1827),key=1827, & 
& nodecnc=[1279,2253,1421,1706,3851,4825,3993,4278,20642, 20641,5628, 5627,21632, 21631,22365, 22366,20646 & 
& , 20645,5636, 5635,21636, 21635,22367, 22368], & 
& edgecnc=[7749,242,8244,8611,7751,246,8246,8612], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1827),elname="xbrick",eltype="xbrick",typekey=1827) 

        call prepare(lib_xbrick(1828),key=1828, & 
& nodecnc=[1237,204,1771,1620,3809,2776,4343,4192,19200, 19199,22369, 22370,19582, 19581,9806, 9805,19206 & 
& , 19205,22371, 22372,19588, 19587,9814, 9813], & 
& edgecnc=[7028,8613,7219,2331,7031,8614,7222,2335], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1828),elname="xbrick",eltype="xbrick",typekey=1828) 

        call prepare(lib_xbrick(1829),key=1829, & 
& nodecnc=[1121,1150,1621,1671,3693,3722,4193,4243,19574, 19573,8518, 8517,19614, 19613,18954, 18953,19578 & 
& , 19577,8526, 8525,19620, 19619,18958, 18957], & 
& edgecnc=[7215,1687,7235,6905,7217,1691,7238,6907], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1829),elname="xbrick",eltype="xbrick",typekey=1829) 

        call prepare(lib_xbrick(1830),key=1830, & 
& nodecnc=[2207,1622,39,402,4779,4194,2611,2974,20266, 20265,21410, 21409,22373, 22374,20344, 20343,20268 & 
& , 20267,21416, 21415,22375, 22376,20348, 20347], & 
& edgecnc=[7561,8133,8615,7600,7562,8136,8616,7602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1830),elname="xbrick",eltype="xbrick",typekey=1830) 

        call prepare(lib_xbrick(1831),key=1831, & 
& nodecnc=[1667,1625,1655,233,4239,4197,4227,2805,21922, 21921,21816, 21815,22377, 22378,21938, 21937 & 
& ,21926, 21925,21820, 21819,22379, 22380,21940, 21939], & 
& edgecnc=[8389,8336,8617,8397,8391,8338,8618,8398], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1831),elname="xbrick",eltype="xbrick",typekey=1831) 

        call prepare(lib_xbrick(1832),key=1832, & 
& nodecnc=[1769,1698,1710,1786,4341,4270,4282,4358,22381, 22382,10090, 10089,9310, 9309,22383, 22384,22385 & 
& , 22386,10096, 10095,9316, 9315,22387, 22388], & 
& edgecnc=[8619,2473,2083,8620,8621,2476,2086,8622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1832),elname="xbrick",eltype="xbrick",typekey=1832) 

        call prepare(lib_xbrick(1833),key=1833, & 
& nodecnc=[1648,1628,1731,216,4220,4200,4303,2788,10842, 10841,22389, 22390,22391, 22392,22393, 22394 & 
& ,10850, 10849,22395, 22396,22397, 22398,22399, 22400], & 
& edgecnc=[2849,8623,8624,8625,2853,8626,8627,8628], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1833),elname="xbrick",eltype="xbrick",typekey=1833) 

        call prepare(lib_xbrick(1834),key=1834, & 
& nodecnc=[1731,1628,1412,182,4303,4200,3984,2754,22390, 22389,21612, 21611,10830, 10829,11882, 11881 & 
& ,22396, 22395,21616, 21615,10838, 10837,11890, 11889], & 
& edgecnc=[8623,8234,2843,3369,8626,8236,2847,3373], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1834),elname="xbrick",eltype="xbrick",typekey=1834) 

        call prepare(lib_xbrick(1835),key=1835, & 
& nodecnc=[151,1662,1753,1629,2723,4234,4325,4201,11434, 11433,22401, 22402,21798, 21797,9338, 9337,11438 & 
& , 11437,22403, 22404,21802, 21801,9344, 9343], & 
& edgecnc=[3145,8629,8327,2097,3147,8630,8329,2100], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1835),elname="xbrick",eltype="xbrick",typekey=1835) 

        call prepare(lib_xbrick(1836),key=1836, & 
& nodecnc=[2470,101,1643,2113,5042,2673,4215,4685,22405, 22406,22407, 22408,22409, 22410,22411, 22412 & 
& ,22413, 22414,22415, 22416,22417, 22418,22419, 22420], & 
& edgecnc=[8631,8632,8633,8634,8635,8636,8637,8638], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1836),elname="xbrick",eltype="xbrick",typekey=1836) 

        call prepare(lib_xbrick(1837),key=1837, & 
& nodecnc=[1367,1642,1325,1631,3939,4214,3897,4203,20912, 20911,21002, 21001,20352, 20351,9900, 9899,20916 & 
& , 20915,21006, 21005,20358, 20357,9908, 9907], & 
& edgecnc=[7884,7929,7604,2378,7886,7931,7607,2382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1837),elname="xbrick",eltype="xbrick",typekey=1837) 

        call prepare(lib_xbrick(1838),key=1838, & 
& nodecnc=[1495,1775,347,1462,4067,4347,2919,4034,5576, 5575,22421, 22422,21434, 21433,13842, 13841,5584 & 
& , 5583,22423, 22424,21440, 21439,13846, 13845], & 
& edgecnc=[216,8639,8145,4349,220,8640,8148,4351], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1838),elname="xbrick",eltype="xbrick",typekey=1838) 

        call prepare(lib_xbrick(1839),key=1839, & 
& nodecnc=[347,1775,1633,2232,2919,4347,4205,4804,22422, 22421,21864, 21863,20650, 20649,22425, 22426 & 
& ,22424, 22423,21868, 21867,20656, 20655,22427, 22428], & 
& edgecnc=[8639,8360,7753,8641,8640,8362,7756,8642], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1839),elname="xbrick",eltype="xbrick",typekey=1839) 

        call prepare(lib_xbrick(1840),key=1840, & 
& nodecnc=[271,2289,1634,2304,2843,4861,4206,4876,21338, 21337,21158, 21157,20970, 20969,20866, 20865 & 
& ,21344, 21343,21162, 21161,20972, 20971,20872, 20871], & 
& edgecnc=[8097,8007,7913,7861,8100,8009,7914,7864], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1840),elname="xbrick",eltype="xbrick",typekey=1840) 

        call prepare(lib_xbrick(1841),key=1841, & 
& nodecnc=[1635,1803,1719,352,4207,4375,4291,2924,12886, 12885,22429, 22430,13926, 13925,21842, 21841 & 
& ,12890, 12889,22431, 22432,13932, 13931,21846, 21845], & 
& edgecnc=[3871,8643,4391,8349,3873,8644,4394,8351], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1841),elname="xbrick",eltype="xbrick",typekey=1841) 

        call prepare(lib_xbrick(1842),key=1842, & 
& nodecnc=[1985,319,2372,1860,4557,2891,4944,4432,22433, 22434,22435, 22436,11420, 11419,9356, 9355,22437 & 
& , 22438,22439, 22440,11428, 11427,9364, 9363], & 
& edgecnc=[8645,8646,3138,2106,8647,8648,3142,2110], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1842),elname="xbrick",eltype="xbrick",typekey=1842) 

        call prepare(lib_xbrick(1843),key=1843, & 
& nodecnc=[1729,1636,1268,1260,4301,4208,3840,3832,20538, 20537,8552, 8551,20458, 20457,8240, 8239,20544 & 
& , 20543,8560, 8559,20462, 20461,8248, 8247], & 
& edgecnc=[7697,1704,7657,1548,7700,1708,7659,1552], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1843),elname="xbrick",eltype="xbrick",typekey=1843) 

        call prepare(lib_xbrick(1844),key=1844, & 
& nodecnc=[2471,1937,1658,1895,5043,4509,4230,4467,22441, 22442,12700, 12699,22443, 22444,22445, 22446 & 
& ,22447, 22448,12708, 12707,22449, 22450,22451, 22452], & 
& edgecnc=[8649,3778,8650,8651,8652,3782,8653,8654], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1844),elname="xbrick",eltype="xbrick",typekey=1844) 

        call prepare(lib_xbrick(1845),key=1845, & 
& nodecnc=[1733,1670,203,1806,4305,4242,2775,4378,22453, 22454,22342, 22341,9932, 9931,22455, 22456,22457 & 
& , 22458,22346, 22345,9940, 9939,22459, 22460], & 
& edgecnc=[8655,8599,2394,8656,8657,8601,2398,8658], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1845),elname="xbrick",eltype="xbrick",typekey=1845) 

        call prepare(lib_xbrick(1846),key=1846, & 
& nodecnc=[1292,1290,1641,2377,3864,3862,4213,4949,20602, 20601,20676, 20675,22461, 22462,20690, 20689 & 
& ,20606, 20605,20680, 20679,22463, 22464,20694, 20693], & 
& edgecnc=[7729,7766,8659,7773,7731,7768,8660,7775], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1846),elname="xbrick",eltype="xbrick",typekey=1846) 

        call prepare(lib_xbrick(1847),key=1847, & 
& nodecnc=[2377,1641,1701,302,4949,4213,4273,2874,22462, 22461,10910, 10909,22465, 22466,20692, 20691 & 
& ,22464, 22463,10916, 10915,22467, 22468,20696, 20695], & 
& edgecnc=[8659,2883,8661,7774,8660,2886,8662,7776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1847),elname="xbrick",eltype="xbrick",typekey=1847) 

        call prepare(lib_xbrick(1848),key=1848, & 
& nodecnc=[2506,2545,100,2488,5078,5117,2672,5060,22469, 22470,22471, 22472,22473, 22474,22475, 22476 & 
& ,22477, 22478,22479, 22480,22481, 22482,22483, 22484], & 
& edgecnc=[8663,8664,8665,8666,8667,8668,8669,8670], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1848),elname="xbrick",eltype="xbrick",typekey=1848) 

        call prepare(lib_xbrick(1849),key=1849, & 
& nodecnc=[117,2137,2470,2113,2689,4709,5042,4685,22485, 22486,22487, 22488,22412, 22411,22489, 22490 & 
& ,22491, 22492,22493, 22494,22420, 22419,22495, 22496], & 
& edgecnc=[8671,8672,8634,8673,8674,8675,8638,8676], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1849),elname="xbrick",eltype="xbrick",typekey=1849) 

        call prepare(lib_xbrick(1850),key=1850, & 
& nodecnc=[1644,1755,1866,164,4216,4327,4438,2736,22497, 22498,22499, 22500,11710, 11709,21488, 21487 & 
& ,22501, 22502,22503, 22504,11718, 11717,21492, 21491], & 
& edgecnc=[8677,8678,3283,8172,8679,8680,3287,8174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1850),elname="xbrick",eltype="xbrick",typekey=1850) 

        call prepare(lib_xbrick(1851),key=1851, & 
& nodecnc=[1644,2553,2571,1755,4216,5125,5143,4327,11700, 11699,22505, 22506,7186, 7185,22498, 22497,11706 & 
& , 11705,22507, 22508,7194, 7193,22502, 22501], & 
& edgecnc=[3278,8681,1021,8677,3281,8682,1025,8679], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1851),elname="xbrick",eltype="xbrick",typekey=1851) 

        call prepare(lib_xbrick(1852),key=1852, & 
& nodecnc=[2225,1014,1645,241,4797,3586,4217,2813,18542, 18541,18838, 18837,18830, 18829,19022, 19021 & 
& ,18544, 18543,18842, 18841,18836, 18835,19028, 19027], & 
& edgecnc=[6699,6847,6843,6939,6700,6849,6846,6942], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1852),elname="xbrick",eltype="xbrick",typekey=1852) 

        call prepare(lib_xbrick(1853),key=1853, & 
& nodecnc=[2286,215,1700,1653,4858,2787,4272,4225,21378, 21377,21592, 21591,7286, 7285,21536, 21535,21386 & 
& , 21385,21596, 21595,7294, 7293,21542, 21541], & 
& edgecnc=[8117,8224,1071,8196,8121,8226,1075,8199], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1853),elname="xbrick",eltype="xbrick",typekey=1853) 

        call prepare(lib_xbrick(1854),key=1854, & 
& nodecnc=[2329,1649,304,1239,4901,4221,2876,3811,21638, 21637,20390, 20389,20274, 20273,20398, 20397 & 
& ,21640, 21639,20392, 20391,20278, 20277,20404, 20403], & 
& edgecnc=[8247,7623,7565,7627,8248,7624,7567,7630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1854),elname="xbrick",eltype="xbrick",typekey=1854) 

        call prepare(lib_xbrick(1855),key=1855, & 
& nodecnc=[1650,1415,235,2255,4222,3987,2807,4827,21466, 21465,21602, 21601,10996, 10995,22509, 22510 & 
& ,21470, 21469,21604, 21603,11004, 11003,22511, 22512], & 
& edgecnc=[8161,8229,2926,8683,8163,8230,2930,8684], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1855),elname="xbrick",eltype="xbrick",typekey=1855) 

        call prepare(lib_xbrick(1856),key=1856, & 
& nodecnc=[152,1702,1699,1808,2724,4274,4271,4380,22513, 22514,9086, 9085,8994, 8993,11406, 11405,22515 & 
& , 22516,9094, 9093,9002, 9001,11412, 11411], & 
& edgecnc=[8685,1971,1925,3131,8686,1975,1929,3134], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1856),elname="xbrick",eltype="xbrick",typekey=1856) 

        call prepare(lib_xbrick(1857),key=1857, & 
& nodecnc=[105,1699,1652,1452,2677,4271,4224,4024,10110, 10109,9084, 9083,9200, 9199,9070, 9069,10116 & 
& , 10115,9092, 9091,9206, 9205,9078, 9077], & 
& edgecnc=[2483,1970,2028,1963,2486,1974,2031,1967], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1857),elname="xbrick",eltype="xbrick",typekey=1857) 

        call prepare(lib_xbrick(1858),key=1858, & 
& nodecnc=[233,1861,1788,1674,2805,4433,4360,4246,22517, 22518,12264, 12263,22519, 22520,7382, 7381,22521 & 
& , 22522,12270, 12269,22523, 22524,7390, 7389], & 
& edgecnc=[8687,3560,8688,1119,8689,3563,8690,1123], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1858),elname="xbrick",eltype="xbrick",typekey=1858) 

        call prepare(lib_xbrick(1859),key=1859, & 
& nodecnc=[2214,1656,1466,240,4786,4228,4038,2812,22362, 22361,8378, 8377,13308, 13307,18964, 18963,22364 & 
& , 22363,8384, 8383,13316, 13315,18968, 18967], & 
& edgecnc=[8609,1617,4082,6910,8610,1620,4086,6912], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1859),elname="xbrick",eltype="xbrick",typekey=1859) 

        call prepare(lib_xbrick(1860),key=1860, & 
& nodecnc=[1624,1329,1307,1657,4196,3901,3879,4229,20930, 20929,20684, 20683,20566, 20565,10948, 10947 & 
& ,20936, 20935,20688, 20687,20570, 20569,10956, 10955], & 
& edgecnc=[7893,7770,7711,2902,7896,7772,7713,2906], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1860),elname="xbrick",eltype="xbrick",typekey=1860) 

        call prepare(lib_xbrick(1861),key=1861, & 
& nodecnc=[1819,1198,1219,244,4391,3770,3791,2816,20124, 20123,10424, 10423,13278, 13277,22525, 22526 & 
& ,20128, 20127,10432, 10431,13284, 13283,22527, 22528], & 
& edgecnc=[7490,2640,4067,8691,7492,2644,4070,8692], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1861),elname="xbrick",eltype="xbrick",typekey=1861) 

        call prepare(lib_xbrick(1862),key=1862, & 
& nodecnc=[1658,1796,1756,1895,4230,4368,4328,4467,22529, 22530,6730, 6729,22531, 22532,22444, 22443,22533 & 
& , 22534,6736, 6735,22535, 22536,22450, 22449], & 
& edgecnc=[8693,793,8694,8650,8695,796,8696,8653], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1862),elname="xbrick",eltype="xbrick",typekey=1862) 

        call prepare(lib_xbrick(1863),key=1863, & 
& nodecnc=[1813,1751,1660,1934,4385,4323,4232,4506,22537, 22538,9214, 9213,22539, 22540,22541, 22542,22543 & 
& , 22544,9220, 9219,22545, 22546,22547, 22548], & 
& edgecnc=[8697,2035,8698,8699,8700,2038,8701,8702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1863),elname="xbrick",eltype="xbrick",typekey=1863) 

        call prepare(lib_xbrick(1864),key=1864, & 
& nodecnc=[1934,1660,1698,1769,4506,4232,4270,4341,22540, 22539,10102, 10101,22382, 22381,22549, 22550 & 
& ,22546, 22545,10106, 10105,22386, 22385,22551, 22552], & 
& edgecnc=[8698,2479,8619,8703,8701,2481,8621,8704], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1864),elname="xbrick",eltype="xbrick",typekey=1864) 

        call prepare(lib_xbrick(1865),key=1865, & 
& nodecnc=[1662,2372,1661,1753,4234,4944,4233,4325,11422, 11421,22553, 22554,21822, 21821,22402, 22401 & 
& ,11430, 11429,22555, 22556,21824, 21823,22404, 22403], & 
& edgecnc=[3139,8705,8339,8629,3143,8706,8340,8630], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1865),elname="xbrick",eltype="xbrick",typekey=1865) 

        call prepare(lib_xbrick(1866),key=1866, & 
& nodecnc=[320,1860,2336,1847,2892,4432,4908,4419,9350, 9349,11418, 11417,22557, 22558,9188, 9187,9358 & 
& , 9357,11426, 11425,22559, 22560,9194, 9193], & 
& edgecnc=[2103,3137,8707,2022,2107,3141,8708,2025], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1866),elname="xbrick",eltype="xbrick",typekey=1866) 

        call prepare(lib_xbrick(1867),key=1867, & 
& nodecnc=[1882,1664,2535,118,4454,4236,5107,2690,10354, 10353,22561, 22562,10340, 10339,21396, 21395 & 
& ,10362, 10361,22563, 22564,10348, 10347,21400, 21399], & 
& edgecnc=[2605,8709,2598,8126,2609,8710,2602,8128], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1867),elname="xbrick",eltype="xbrick",typekey=1867) 

        call prepare(lib_xbrick(1868),key=1868, & 
& nodecnc=[2385,2448,1733,2384,4957,5020,4305,4956,22565, 22566,22567, 22568,22569, 22570,22571, 22572 & 
& ,22573, 22574,22575, 22576,22577, 22578,22579, 22580], & 
& edgecnc=[8711,8712,8713,8714,8715,8716,8717,8718], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1868),elname="xbrick",eltype="xbrick",typekey=1868) 

        call prepare(lib_xbrick(1869),key=1869, & 
& nodecnc=[1666,1610,1754,2153,4238,4182,4326,4725,11886, 11885,22581, 22582,22583, 22584,7962, 7961,11894 & 
& , 11893,22585, 22586,22587, 22588,7970, 7969], & 
& edgecnc=[3371,8719,8720,1409,3375,8721,8722,1413], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1869),elname="xbrick",eltype="xbrick",typekey=1869) 

        call prepare(lib_xbrick(1870),key=1870, & 
& nodecnc=[350,1478,1497,1668,2922,4050,4069,4240,21856, 21855,13892, 13891,21954, 21953,13004, 13003 & 
& ,21860, 21859,13896, 13895,21956, 21955,13012, 13011], & 
& edgecnc=[8356,4374,8405,3930,8358,4376,8406,3934], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1870),elname="xbrick",eltype="xbrick",typekey=1870) 

        call prepare(lib_xbrick(1871),key=1871, & 
& nodecnc=[2260,1669,2288,1613,4832,4241,4860,4185,22330, 22329,12836, 12835,22334, 22333,12820, 12819 & 
& ,22332, 22331,12844, 12843,22338, 22337,12826, 12825], & 
& edgecnc=[8593,3846,8595,3838,8594,3850,8597,3841], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1871),elname="xbrick",eltype="xbrick",typekey=1871) 

        call prepare(lib_xbrick(1872),key=1872, & 
& nodecnc=[1733,2448,202,1670,4305,5020,2774,4242,22568, 22567,22589, 22590,22591, 22592,22454, 22453 & 
& ,22576, 22575,22593, 22594,22595, 22596,22458, 22457], & 
& edgecnc=[8712,8723,8724,8655,8716,8725,8726,8657], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1872),elname="xbrick",eltype="xbrick",typekey=1872) 

        call prepare(lib_xbrick(1873),key=1873, & 
& nodecnc=[2219,1734,254,1672,4791,4306,2826,4244,22597, 22598,22599, 22600,22601, 22602,6274, 6273,22603 & 
& , 22604,22605, 22606,22607, 22608,6282, 6281], & 
& edgecnc=[8727,8728,8729,565,8730,8731,8732,569], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1873),elname="xbrick",eltype="xbrick",typekey=1873) 

        call prepare(lib_xbrick(1874),key=1874, & 
& nodecnc=[2218,1736,328,1673,4790,4308,2900,4245,22609, 22610,12608, 12607,22611, 22612,7006, 7005,22613 & 
& , 22614,12614, 12613,22615, 22616,7014, 7013], & 
& edgecnc=[8733,3732,8734,931,8735,3735,8736,935], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1874),elname="xbrick",eltype="xbrick",typekey=1874) 

        call prepare(lib_xbrick(1875),key=1875, & 
& nodecnc=[1674,1788,1810,232,4246,4360,4382,2804,22520, 22519,22617, 22618,12212, 12211,21886, 21885 & 
& ,22524, 22523,22619, 22620,12216, 12215,21888, 21887], & 
& edgecnc=[8688,8737,3534,8371,8690,8738,3536,8372], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1875),elname="xbrick",eltype="xbrick",typekey=1875) 

        call prepare(lib_xbrick(1876),key=1876, & 
& nodecnc=[1676,1782,1842,258,4248,4354,4414,2830,7630, 7629,22621, 22622,7616, 7615,7674, 7673,7636, 7635 & 
& ,22623, 22624,7624, 7623,7680, 7679], & 
& edgecnc=[1243,8739,1236,1265,1246,8740,1240,1268], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1876),elname="xbrick",eltype="xbrick",typekey=1876) 

        call prepare(lib_xbrick(1877),key=1877, & 
& nodecnc=[1679,1781,1841,324,4251,4353,4413,2896,13050, 13049,22625, 22626,11448, 11447,5924, 5923,13056 & 
& , 13055,22627, 22628,11456, 11455,5930, 5929], & 
& edgecnc=[3953,8741,3152,390,3956,8742,3156,393], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1877),elname="xbrick",eltype="xbrick",typekey=1877) 

        call prepare(lib_xbrick(1878),key=1878, & 
& nodecnc=[1980,93,1680,1738,4552,2665,4252,4310,22629, 22630,21982, 21981,22631, 22632,7460, 7459,22633 & 
& , 22634,21984, 21983,22635, 22636,7468, 7467], & 
& edgecnc=[8743,8419,8744,1158,8745,8420,8746,1162], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1878),elname="xbrick",eltype="xbrick",typekey=1878) 

        call prepare(lib_xbrick(1879),key=1879, & 
& nodecnc=[1684,1783,1724,325,4256,4355,4296,2897,22637, 22638,7524, 7523,22318, 22317,13576, 13575,22639 & 
& , 22640,7530, 7529,22320, 22319,13582, 13581], & 
& edgecnc=[8747,1190,8587,4216,8748,1193,8588,4219], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1879),elname="xbrick",eltype="xbrick",typekey=1879) 

        call prepare(lib_xbrick(1880),key=1880, & 
& nodecnc=[1685,1784,1725,257,4257,4356,4297,2829,22641, 22642,6046, 6045,22310, 22309,14060, 14059,22643 & 
& , 22644,6054, 6053,22312, 22311,14064, 14063], & 
& edgecnc=[8749,451,8583,4458,8750,455,8584,4460], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1880),elname="xbrick",eltype="xbrick",typekey=1880) 

        call prepare(lib_xbrick(1881),key=1881, & 
& nodecnc=[246,1746,1992,1997,2818,4318,4564,4569,22645, 22646,12530, 12529,22647, 22648,22038, 22037 & 
& ,22649, 22650,12536, 12535,22651, 22652,22044, 22043], & 
& edgecnc=[8751,3693,8752,8447,8753,3696,8754,8450], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1881),elname="xbrick",eltype="xbrick",typekey=1881) 

        call prepare(lib_xbrick(1882),key=1882, & 
& nodecnc=[173,1737,2220,1686,2745,4309,4792,4258,11800, 11799,22653, 22654,22076, 22075,22655, 22656 & 
& ,11804, 11803,22657, 22658,22080, 22079,22659, 22660], & 
& edgecnc=[3328,8755,8466,8756,3330,8757,8468,8758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1882),elname="xbrick",eltype="xbrick",typekey=1882) 

        call prepare(lib_xbrick(1883),key=1883, & 
& nodecnc=[2245,173,1686,1932,4817,2745,4258,4504,7474, 7473,22656, 22655,22216, 22215,22200, 22199,7482 & 
& , 7481,22660, 22659,22220, 22219,22208, 22207], & 
& edgecnc=[1165,8756,8536,8528,1169,8758,8538,8532], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1883),elname="xbrick",eltype="xbrick",typekey=1883) 

        call prepare(lib_xbrick(1884),key=1884, & 
& nodecnc=[1632,337,1730,1747,4204,2909,4302,4319,21662, 21661,21656, 21655,7160, 7159,10914, 10913,21664 & 
& , 21663,21660, 21659,7168, 7167,10920, 10919], & 
& edgecnc=[8259,8256,1008,2885,8260,8258,1012,2888], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1884),elname="xbrick",eltype="xbrick",typekey=1884) 

        call prepare(lib_xbrick(1885),key=1885, & 
& nodecnc=[109,1735,2221,1688,2681,4307,4793,4260,9656, 9655,22661, 22662,13100, 13099,22262, 22261,9664 & 
& , 9663,22663, 22664,13104, 13103,22268, 22267], & 
& edgecnc=[2256,8759,3978,8559,2260,8760,3980,8562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1885),elname="xbrick",eltype="xbrick",typekey=1885) 

        call prepare(lib_xbrick(1886),key=1886, & 
& nodecnc=[1823,252,2066,1742,4395,2824,4638,4314,12414, 12413,22166, 22165,22665, 22666,22667, 22668 & 
& ,12420, 12419,22168, 22167,22669, 22670,22671, 22672], & 
& edgecnc=[3635,8511,8761,8762,3638,8512,8763,8764], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1886),elname="xbrick",eltype="xbrick",typekey=1886) 

        call prepare(lib_xbrick(1887),key=1887, & 
& nodecnc=[1822,330,2065,1743,4394,2902,4637,4315,10640, 10639,22234, 22233,10522, 10521,10532, 10531 & 
& ,10648, 10647,22236, 22235,10528, 10527,10540, 10539], & 
& edgecnc=[2748,8545,2689,2694,2752,8546,2692,2698], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1887),elname="xbrick",eltype="xbrick",typekey=1887) 

        call prepare(lib_xbrick(1888),key=1888, & 
& nodecnc=[1825,226,1694,1740,4397,2798,4266,4312,12190, 12189,13480, 13479,7140, 7139,6880, 6879,12196 & 
& , 12195,13488, 13487,7148, 7147,6888, 6887], & 
& edgecnc=[3523,4168,998,868,3526,4172,1002,872], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1888),elname="xbrick",eltype="xbrick",typekey=1888) 

        call prepare(lib_xbrick(1889),key=1889, & 
& nodecnc=[1824,355,1695,1741,4396,2927,4267,4313,13038, 13037,22296, 22295,9176, 9175,9116, 9115,13044 & 
& , 13043,22300, 22299,9184, 9183,9124, 9123], & 
& edgecnc=[3947,8576,2016,1986,3950,8578,2020,1990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1889),elname="xbrick",eltype="xbrick",typekey=1889) 

        call prepare(lib_xbrick(1890),key=1890, & 
& nodecnc=[1805,1700,1648,216,4377,4272,4220,2788,7288, 7287,21590, 21589,22394, 22393,22673, 22674,7296 & 
& , 7295,21594, 21593,22400, 22399,22675, 22676], & 
& edgecnc=[1072,8223,8625,8765,1076,8225,8628,8766], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1890),elname="xbrick",eltype="xbrick",typekey=1890) 

        call prepare(lib_xbrick(1891),key=1891, & 
& nodecnc=[1796,1658,302,1701,4368,4230,2874,4273,22530, 22529,12698, 12697,22466, 22465,22677, 22678 & 
& ,22534, 22533,12706, 12705,22468, 22467,22679, 22680], & 
& edgecnc=[8693,3777,8661,8767,8695,3781,8662,8768], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1891),elname="xbrick",eltype="xbrick",typekey=1891) 

        call prepare(lib_xbrick(1892),key=1892, & 
& nodecnc=[1653,1704,1795,1703,4225,4276,4367,4275,7284, 7283,22681, 22682,12266, 12265,21538, 21537,7292 & 
& , 7291,22683, 22684,12272, 12271,21544, 21543], & 
& edgecnc=[1070,8769,3561,8197,1074,8770,3564,8200], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1892),elname="xbrick",eltype="xbrick",typekey=1892) 

        call prepare(lib_xbrick(1893),key=1893, & 
& nodecnc=[2234,1861,233,1655,4806,4433,2805,4227,7338, 7337,22518, 22517,22378, 22377,21814, 21813,7346 & 
& , 7345,22522, 22521,22380, 22379,21818, 21817], & 
& edgecnc=[1097,8687,8617,8335,1101,8689,8618,8337], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1893),elname="xbrick",eltype="xbrick",typekey=1893) 

        call prepare(lib_xbrick(1894),key=1894, & 
& nodecnc=[1795,1704,1917,266,4367,4276,4489,2838,22682, 22681,12480, 12479,22685, 22686,22687, 22688 & 
& ,22684, 22683,12486, 12485,22689, 22690,22691, 22692], & 
& edgecnc=[8769,3668,8771,8772,8770,3671,8773,8774], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1894),elname="xbrick",eltype="xbrick",typekey=1894) 

        call prepare(lib_xbrick(1895),key=1895, & 
& nodecnc=[2254,1279,1706,2232,4826,3851,4278,4804,20594, 20593,22366, 22365,22693, 22694,20654, 20653 & 
& ,20598, 20597,22368, 22367,22695, 22696,20660, 20659], & 
& edgecnc=[7725,8611,8775,7755,7727,8612,8776,7758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1895),elname="xbrick",eltype="xbrick",typekey=1895) 

        call prepare(lib_xbrick(1896),key=1896, & 
& nodecnc=[2232,1706,1797,347,4804,4278,4369,2919,22694, 22693,21630, 21629,21430, 21429,22426, 22425 & 
& ,22696, 22695,21634, 21633,21436, 21435,22428, 22427], & 
& edgecnc=[8775,8243,8143,8641,8776,8245,8146,8642], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1896),elname="xbrick",eltype="xbrick",typekey=1896) 

        call prepare(lib_xbrick(1897),key=1897, & 
& nodecnc=[2129,2548,2057,2093,4701,5120,4629,4665,22697, 22698,22699, 22700,22701, 22702,22703, 22704 & 
& ,22705, 22706,22707, 22708,22709, 22710,22711, 22712], & 
& edgecnc=[8777,8778,8779,8780,8781,8782,8783,8784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1897),elname="xbrick",eltype="xbrick",typekey=1897) 

        call prepare(lib_xbrick(1898),key=1898, & 
& nodecnc=[1707,2113,1643,1630,4279,4685,4215,4202,22713, 22714,22410, 22409,9994, 9993,22715, 22716,22717 & 
& , 22718,22418, 22417,10000, 9999,22719, 22720], & 
& edgecnc=[8785,8633,2425,8786,8787,8637,2428,8788], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1898),elname="xbrick",eltype="xbrick",typekey=1898) 

        call prepare(lib_xbrick(1899),key=1899, & 
& nodecnc=[1996,1875,2496,1931,4568,4447,5068,4503,22721, 22722,22723, 22724,22725, 22726,22727, 22728 & 
& ,22729, 22730,22731, 22732,22733, 22734,22735, 22736], & 
& edgecnc=[8789,8790,8791,8792,8793,8794,8795,8796], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1899),elname="xbrick",eltype="xbrick",typekey=1899) 

        call prepare(lib_xbrick(1900),key=1900, & 
& nodecnc=[1896,1820,2279,1757,4468,4392,4851,4329,22737, 22738,22739, 22740,12542, 12541,22741, 22742 & 
& ,22743, 22744,22745, 22746,12546, 12545,22747, 22748], & 
& edgecnc=[8797,8798,3699,8799,8800,8801,3701,8802], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1900),elname="xbrick",eltype="xbrick",typekey=1900) 

        call prepare(lib_xbrick(1901),key=1901, & 
& nodecnc=[1401,2230,234,1717,3973,4802,2806,4289,21346, 21345,21924, 21923,12218, 12217,21478, 21477 & 
& ,21352, 21351,21928, 21927,12224, 12223,21480, 21479], & 
& edgecnc=[8101,8390,3537,8167,8104,8392,3540,8168], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1901),elname="xbrick",eltype="xbrick",typekey=1901) 

        call prepare(lib_xbrick(1902),key=1902, & 
& nodecnc=[229,1800,1758,2226,2801,4372,4330,4798,21946, 21945,6804, 6803,22749, 22750,22751, 22752,21948 & 
& , 21947,6812, 6811,22753, 22754,22755, 22756], & 
& edgecnc=[8401,830,8803,8804,8402,834,8805,8806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1902),elname="xbrick",eltype="xbrick",typekey=1902) 

        call prepare(lib_xbrick(1903),key=1903, & 
& nodecnc=[1973,1836,2228,1776,4545,4408,4800,4348,22757, 22758,22759, 22760,7242, 7241,22761, 22762,22763 & 
& , 22764,22765, 22766,7250, 7249,22767, 22768], & 
& edgecnc=[8807,8808,1049,8809,8810,8811,1053,8812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1903),elname="xbrick",eltype="xbrick",typekey=1903) 

        call prepare(lib_xbrick(1904),key=1904, & 
& nodecnc=[1809,1793,216,1731,4381,4365,2788,4303,7220, 7219,22769, 22770,22392, 22391,12128, 12127,7226 & 
& , 7225,22771, 22772,22398, 22397,12132, 12131], & 
& edgecnc=[1038,8813,8624,3492,1041,8814,8627,3494], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1904),elname="xbrick",eltype="xbrick",typekey=1904) 

        call prepare(lib_xbrick(1905),key=1905, & 
& nodecnc=[2449,2448,2385,1863,5021,5020,4957,4435,22773, 22774,22566, 22565,22775, 22776,12002, 12001 & 
& ,22777, 22778,22574, 22573,22779, 22780,12008, 12007], & 
& edgecnc=[8815,8711,8816,3429,8817,8715,8818,3432], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1905),elname="xbrick",eltype="xbrick",typekey=1905) 

        call prepare(lib_xbrick(1906),key=1906, & 
& nodecnc=[1812,2384,1733,1806,4384,4956,4305,4378,22781, 22782,22570, 22569,22456, 22455,9930, 9929,22783 & 
& , 22784,22578, 22577,22460, 22459,9938, 9937], & 
& edgecnc=[8819,8713,8656,2393,8820,8717,8658,2397], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1906),elname="xbrick",eltype="xbrick",typekey=1906) 

        call prepare(lib_xbrick(1907),key=1907, & 
& nodecnc=[1774,1818,1819,244,4346,4390,4391,2816,22785, 22786,22787, 22788,22526, 22525,22144, 22143 & 
& ,22789, 22790,22791, 22792,22528, 22527,22148, 22147], & 
& edgecnc=[8821,8822,8691,8500,8823,8824,8692,8502], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1907),elname="xbrick",eltype="xbrick",typekey=1907) 

        call prepare(lib_xbrick(1908),key=1908, & 
& nodecnc=[1831,1734,2219,255,4403,4306,4791,2827,6076, 6075,22598, 22597,22018, 22017,12586, 12585,6084 & 
& , 6083,22604, 22603,22020, 22019,12592, 12591], & 
& edgecnc=[466,8727,8437,3721,470,8730,8438,3724], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1908),elname="xbrick",eltype="xbrick",typekey=1908) 

        call prepare(lib_xbrick(1909),key=1909, & 
& nodecnc=[1901,1871,254,1734,4473,4443,2826,4306,12564, 12563,6088, 6087,22600, 22599,6074, 6073,12570 & 
& , 12569,6096, 6095,22606, 22605,6082, 6081], & 
& edgecnc=[3710,472,8728,465,3713,476,8731,469], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1909),elname="xbrick",eltype="xbrick",typekey=1909) 

        call prepare(lib_xbrick(1910),key=1910, & 
& nodecnc=[2221,1735,1830,191,4793,4307,4402,2763,22662, 22661,22793, 22794,6160, 6159,6172, 6171,22664 & 
& , 22663,22795, 22796,6168, 6167,6180, 6179], & 
& edgecnc=[8759,8825,508,514,8760,8826,512,518], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1910),elname="xbrick",eltype="xbrick",typekey=1910) 

        call prepare(lib_xbrick(1911),key=1911, & 
& nodecnc=[1829,2360,1899,1736,4401,4932,4471,4308,22797, 22798,22799, 22800,12610, 12609,22801, 22802 & 
& ,22803, 22804,22805, 22806,12616, 12615,22807, 22808], & 
& edgecnc=[8827,8828,3733,8829,8830,8831,3736,8832], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1911),elname="xbrick",eltype="xbrick",typekey=1911) 

        call prepare(lib_xbrick(1912),key=1912, & 
& nodecnc=[1829,1736,2218,327,4401,4308,4790,2899,22802, 22801,22610, 22609,22058, 22057,12618, 12617 & 
& ,22808, 22807,22614, 22613,22060, 22059,12622, 12621], & 
& edgecnc=[8829,8733,8457,3737,8832,8735,8458,3739], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1912),elname="xbrick",eltype="xbrick",typekey=1912) 

        call prepare(lib_xbrick(1913),key=1913, & 
& nodecnc=[2220,1737,1828,124,4792,4309,4400,2696,22654, 22653,22809, 22810,10452, 10451,7784, 7783,22658 & 
& , 22657,22811, 22812,10456, 10455,7792, 7791], & 
& edgecnc=[8755,8833,2654,1320,8757,8834,2656,1324], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1913),elname="xbrick",eltype="xbrick",typekey=1913) 

        call prepare(lib_xbrick(1914),key=1914, & 
& nodecnc=[91,1827,1738,1680,2663,4399,4310,4252,7698, 7697,7690, 7689,22632, 22631,7726, 7725,7704, 7703 & 
& ,7696, 7695,22636, 22635,7732, 7731], & 
& edgecnc=[1277,1273,8744,1291,1280,1276,8746,1294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1914),elname="xbrick",eltype="xbrick",typekey=1914) 

        call prepare(lib_xbrick(1915),key=1915, & 
& nodecnc=[2278,1979,1739,1894,4850,4551,4311,4466,22813, 22814,10182, 10181,22815, 22816,22817, 22818 & 
& ,22819, 22820,10190, 10189,22821, 22822,22823, 22824], & 
& edgecnc=[8835,2519,8836,8837,8838,2523,8839,8840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1915),elname="xbrick",eltype="xbrick",typekey=1915) 

        call prepare(lib_xbrick(1916),key=1916, & 
& nodecnc=[156,1826,1739,1681,2728,4398,4311,4253,11458, 11457,22825, 22826,10180, 10179,9578, 9577,11464 & 
& , 11463,22827, 22828,10188, 10187,9584, 9583], & 
& edgecnc=[3157,8841,2518,2217,3160,8842,2522,2220], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1916),elname="xbrick",eltype="xbrick",typekey=1916) 

        call prepare(lib_xbrick(1917),key=1917, & 
& nodecnc=[261,1893,1740,1978,2833,4465,4312,4550,22829, 22830,6882, 6881,7138, 7137,22831, 22832,22833 & 
& , 22834,6890, 6889,7146, 7145,22835, 22836], & 
& edgecnc=[8843,869,997,8844,8845,873,1001,8846], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1917),elname="xbrick",eltype="xbrick",typekey=1917) 

        call prepare(lib_xbrick(1918),key=1918, & 
& nodecnc=[321,1892,1741,1977,2893,4464,4313,4549,12898, 12897,9118, 9117,9174, 9173,22837, 22838,12906 & 
& , 12905,9126, 9125,9182, 9181,22839, 22840], & 
& edgecnc=[3877,1987,2015,8847,3881,1991,2019,8848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1918),elname="xbrick",eltype="xbrick",typekey=1918) 

        call prepare(lib_xbrick(1919),key=1919, & 
& nodecnc=[253,1976,1742,2066,2825,4548,4314,4638,22841, 22842,5972, 5971,22666, 22665,6246, 6245,22843 & 
& , 22844,5980, 5979,22670, 22669,6252, 6251], & 
& edgecnc=[8849,414,8761,551,8850,418,8763,554], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1919),elname="xbrick",eltype="xbrick",typekey=1919) 

        call prepare(lib_xbrick(1920),key=1920, & 
& nodecnc=[2355,1743,1975,1890,4927,4315,4547,4462,10534, 10533,10520, 10519,22845, 22846,22847, 22848 & 
& ,10542, 10541,10526, 10525,22849, 22850,22851, 22852], & 
& edgecnc=[2695,2688,8851,8852,2699,2691,8853,8854], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1920),elname="xbrick",eltype="xbrick",typekey=1920) 

        call prepare(lib_xbrick(1921),key=1921, & 
& nodecnc=[2226,1821,1889,1744,4798,4393,4461,4316,22853, 22854,22855, 22856,7952, 7951,22857, 22858,22859 & 
& , 22860,22861, 22862,7958, 7957,22863, 22864], & 
& edgecnc=[8855,8856,1404,8857,8858,8859,1407,8860], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1921),elname="xbrick",eltype="xbrick",typekey=1921) 

        call prepare(lib_xbrick(1922),key=1922, & 
& nodecnc=[2226,1744,1682,229,4798,4316,4254,2801,22858, 22857,7934, 7933,6818, 6817,22752, 22751,22864 & 
& , 22863,7942, 7941,6826, 6825,22756, 22755], & 
& edgecnc=[8857,1395,837,8804,8860,1399,841,8806], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1922),elname="xbrick",eltype="xbrick",typekey=1922) 

        call prepare(lib_xbrick(1923),key=1923, & 
& nodecnc=[2261,1745,1683,353,4833,4317,4255,2925,22865, 22866,22867, 22868,9422, 9421,22869, 22870,22871 & 
& , 22872,22873, 22874,9428, 9427,22875, 22876], & 
& edgecnc=[8861,8862,2139,8863,8864,8865,2142,8866], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1923),elname="xbrick",eltype="xbrick",typekey=1923) 

        call prepare(lib_xbrick(1924),key=1924, & 
& nodecnc=[2018,1994,320,1984,4590,4566,2892,4556,9384, 9383,22877, 22878,9186, 9185,9398, 9397,9390, 9389 & 
& ,22879, 22880,9192, 9191,9404, 9403], & 
& edgecnc=[2120,8867,2021,2127,2123,8868,2024,2130], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1924),elname="xbrick",eltype="xbrick",typekey=1924) 

        call prepare(lib_xbrick(1925),key=1925, & 
& nodecnc=[1746,2295,280,1887,4318,4867,2852,4459,22881, 22882,22883, 22884,22885, 22886,12532, 12531 & 
& ,22887, 22888,22889, 22890,22891, 22892,12538, 12537], & 
& edgecnc=[8869,8870,8871,3694,8872,8873,8874,3697], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1925),elname="xbrick",eltype="xbrick",typekey=1925) 

        call prepare(lib_xbrick(1926),key=1926, & 
& nodecnc=[1811,1748,1240,339,4383,4320,3812,2911,8566, 8565,20466, 20465,19786, 19785,21904, 21903,8570 & 
& , 8569,20468, 20467,19792, 19791,21908, 21907], & 
& edgecnc=[1711,7661,7321,8380,1713,7662,7324,8382], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1926),elname="xbrick",eltype="xbrick",typekey=1926) 

        call prepare(lib_xbrick(1927),key=1927, & 
& nodecnc=[319,1985,2261,1749,2891,4557,4833,4321,22434, 22433,22893, 22894,12876, 12875,22895, 22896 & 
& ,22438, 22437,22897, 22898,12884, 12883,22899, 22900], & 
& edgecnc=[8645,8875,3866,8876,8647,8877,3870,8878], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1927),elname="xbrick",eltype="xbrick",typekey=1927) 

        call prepare(lib_xbrick(1928),key=1928, & 
& nodecnc=[204,1663,1732,1771,2776,4235,4304,4343,20194, 20193,10412, 10411,22901, 22902,22370, 22369 & 
& ,20196, 20195,10420, 10419,22903, 22904,22372, 22371], & 
& edgecnc=[7525,2634,8879,8613,7526,2638,8880,8614], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1928),elname="xbrick",eltype="xbrick",typekey=1928) 

        call prepare(lib_xbrick(1929),key=1929, & 
& nodecnc=[2535,1664,1750,1865,5107,4236,4322,4437,22562, 22561,20450, 20449,22905, 22906,21270, 21269 & 
& ,22564, 22563,20454, 20453,22907, 22908,21276, 21275], & 
& edgecnc=[8709,7653,8881,8063,8710,7655,8882,8066], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1929),elname="xbrick",eltype="xbrick",typekey=1929) 

        call prepare(lib_xbrick(1930),key=1930, & 
& nodecnc=[1865,1750,1670,202,4437,4322,4242,2774,22906, 22905,22344, 22343,22592, 22591,22909, 22910 & 
& ,22908, 22907,22348, 22347,22596, 22595,22911, 22912], & 
& edgecnc=[8881,8600,8724,8883,8882,8602,8726,8884], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1930),elname="xbrick",eltype="xbrick",typekey=1930) 

        call prepare(lib_xbrick(1931),key=1931, & 
& nodecnc=[1751,1813,1770,105,4323,4385,4342,2677,22538, 22537,22913, 22914,10112, 10111,9068, 9067,22544 & 
& , 22543,22915, 22916,10118, 10117,9076, 9075], & 
& edgecnc=[8697,8885,2484,1962,8700,8886,2487,1966], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1931),elname="xbrick",eltype="xbrick",typekey=1931) 

        call prepare(lib_xbrick(1932),key=1932, & 
& nodecnc=[2552,2105,166,1754,5124,4677,2738,4326,6762, 6761,22917, 22918,22919, 22920,22921, 22922,6770 & 
& , 6769,22923, 22924,22925, 22926,22927, 22928], & 
& edgecnc=[809,8887,8888,8889,813,8890,8891,8892], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1932),elname="xbrick",eltype="xbrick",typekey=1932) 

        call prepare(lib_xbrick(1933),key=1933, & 
& nodecnc=[1804,2547,131,1755,4376,5119,2703,4327,6748, 6747,22929, 22930,22931, 22932,7192, 7191,6756 & 
& , 6755,22933, 22934,22935, 22936,7200, 7199], & 
& edgecnc=[802,8893,8894,1024,806,8895,8896,1028], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1933),elname="xbrick",eltype="xbrick",typekey=1933) 

        call prepare(lib_xbrick(1934),key=1934, & 
& nodecnc=[301,1895,1756,1834,2873,4467,4328,4406,22937, 22938,22532, 22531,22939, 22940,22941, 22942 & 
& ,22943, 22944,22536, 22535,22945, 22946,22947, 22948], & 
& edgecnc=[8897,8694,8898,8899,8900,8696,8901,8902], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1934),elname="xbrick",eltype="xbrick",typekey=1934) 

        call prepare(lib_xbrick(1935),key=1935, & 
& nodecnc=[1834,1756,1716,335,4406,4328,4288,2907,22940, 22939,6734, 6733,22242, 22241,8194, 8193,22946 & 
& , 22945,6740, 6739,22244, 22243,8200, 8199], & 
& edgecnc=[8898,795,8549,1525,8901,798,8550,1528], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1935),elname="xbrick",eltype="xbrick",typekey=1935) 

        call prepare(lib_xbrick(1936),key=1936, & 
& nodecnc=[2295,1820,1896,1987,4867,4392,4468,4559,22949, 22950,22738, 22737,5692, 5691,22951, 22952,22953 & 
& , 22954,22744, 22743,5700, 5699,22955, 22956], & 
& edgecnc=[8903,8797,274,8904,8905,8800,278,8906], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1936),elname="xbrick",eltype="xbrick",typekey=1936) 

        call prepare(lib_xbrick(1937),key=1937, & 
& nodecnc=[281,1896,1757,1835,2853,4468,4329,4407,5686, 5685,22742, 22741,5704, 5703,22957, 22958,5694 & 
& , 5693,22748, 22747,5712, 5711,22959, 22960], & 
& edgecnc=[271,8799,280,8907,275,8802,284,8908], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1937),elname="xbrick",eltype="xbrick",typekey=1937) 

        call prepare(lib_xbrick(1938),key=1938, & 
& nodecnc=[2226,1758,2002,1821,4798,4330,4574,4393,22750, 22749,12198, 12197,22961, 22962,22854, 22853 & 
& ,22754, 22753,12204, 12203,22963, 22964,22860, 22859], & 
& edgecnc=[8803,3527,8909,8855,8805,3530,8910,8858], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1938),elname="xbrick",eltype="xbrick",typekey=1938) 

        call prepare(lib_xbrick(1939),key=1939, & 
& nodecnc=[2228,1836,1902,2227,4800,4408,4474,4799,22760, 22759,22965, 22966,12200, 12199,22967, 22968 & 
& ,22766, 22765,22969, 22970,12206, 12205,22971, 22972], & 
& edgecnc=[8808,8911,3528,8912,8811,8913,3531,8914], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1939),elname="xbrick",eltype="xbrick",typekey=1939) 

        call prepare(lib_xbrick(1940),key=1940, & 
& nodecnc=[297,1837,1759,1905,2869,4409,4331,4477,22973, 22974,10652, 10651,22975, 22976,22977, 22978 & 
& ,22979, 22980,10656, 10655,22981, 22982,22983, 22984], & 
& edgecnc=[8915,2754,8916,8917,8918,2756,8919,8920], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1940),elname="xbrick",eltype="xbrick",typekey=1940) 

        call prepare(lib_xbrick(1941),key=1941, & 
& nodecnc=[1993,1905,1759,1822,4565,4477,4331,4394,22985, 22986,22976, 22975,10634, 10633,10530, 10529 & 
& ,22987, 22988,22982, 22981,10642, 10641,10538, 10537], & 
& edgecnc=[8921,8916,2745,2693,8922,8919,2749,2697], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1941),elname="xbrick",eltype="xbrick",typekey=1941) 

        call prepare(lib_xbrick(1942),key=1942, & 
& nodecnc=[1823,1906,2380,1760,4395,4478,4952,4332,5788, 5787,22989, 22990,22991, 22992,12410, 12409,5796 & 
& , 5795,22993, 22994,22995, 22996,12416, 12415], & 
& edgecnc=[322,8923,8924,3633,326,8925,8926,3636], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1942),elname="xbrick",eltype="xbrick",typekey=1942) 

        call prepare(lib_xbrick(1943),key=1943, & 
& nodecnc=[323,1839,1761,1923,2895,4411,4333,4495,22997, 22998,9030, 9029,22999, 23000,23001, 23002,23003 & 
& , 23004,9036, 9035,23005, 23006,23007, 23008], & 
& edgecnc=[8927,1943,8928,8929,8930,1946,8931,8932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1943),elname="xbrick",eltype="xbrick",typekey=1943) 

        call prepare(lib_xbrick(1944),key=1944, & 
& nodecnc=[322,1923,1761,1824,2894,4495,4333,4396,12914, 12913,23000, 22999,13034, 13033,9114, 9113,12922 & 
& , 12921,23006, 23005,13040, 13039,9122, 9121], & 
& edgecnc=[3885,8928,3945,1985,3889,8931,3948,1989], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1944),elname="xbrick",eltype="xbrick",typekey=1944) 

        call prepare(lib_xbrick(1945),key=1945, & 
& nodecnc=[2299,1826,1762,1907,4871,4398,4334,4479,23009, 23010,11462, 11461,23011, 23012,23013, 23014 & 
& ,23015, 23016,11468, 11467,23017, 23018,23019, 23020], & 
& edgecnc=[8933,3159,8934,8935,8936,3162,8937,8938], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1945),elname="xbrick",eltype="xbrick",typekey=1945) 

        call prepare(lib_xbrick(1946),key=1946, & 
& nodecnc=[323,1907,1762,1841,2895,4479,4334,4413,23021, 23022,23012, 23011,11442, 11441,23023, 23024 & 
& ,23025, 23026,23018, 23017,11450, 11449,23027, 23028], & 
& edgecnc=[8939,8934,3149,8940,8941,8937,3153,8942], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1946),elname="xbrick",eltype="xbrick",typekey=1946) 

        call prepare(lib_xbrick(1947),key=1947, & 
& nodecnc=[259,1840,1763,1922,2831,4412,4335,4494,23029, 23030,7432, 7431,23031, 23032,23033, 23034,23035 & 
& , 23036,7440, 7439,23037, 23038,23039, 23040], & 
& edgecnc=[8943,1144,8944,8945,8946,1148,8947,8948], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1947),elname="xbrick",eltype="xbrick",typekey=1947) 

        call prepare(lib_xbrick(1948),key=1948, & 
& nodecnc=[260,1922,1763,1825,2832,4494,4335,4397,12466, 12465,23032, 23031,12186, 12185,6878, 6877,12474 & 
& , 12473,23038, 23037,12192, 12191,6886, 6885], & 
& edgecnc=[3661,8944,3521,867,3665,8947,3524,871], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1948),elname="xbrick",eltype="xbrick",typekey=1948) 

        call prepare(lib_xbrick(1949),key=1949, & 
& nodecnc=[2300,1827,1764,1908,4872,4399,4336,4480,7686, 7685,7702, 7701,23041, 23042,23043, 23044,7692 & 
& , 7691,7708, 7707,23045, 23046,23047, 23048], & 
& edgecnc=[1271,1279,8949,8950,1274,1282,8951,8952], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1949),elname="xbrick",eltype="xbrick",typekey=1949) 

        call prepare(lib_xbrick(1950),key=1950, & 
& nodecnc=[259,1908,1764,1842,2831,4480,4336,4414,23049, 23050,23042, 23041,7610, 7609,23051, 23052,23053 & 
& , 23054,23046, 23045,7618, 7617,23055, 23056], & 
& edgecnc=[8953,8949,1233,8954,8955,8951,1237,8956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1950),elname="xbrick",eltype="xbrick",typekey=1950) 

        call prepare(lib_xbrick(1951),key=1951, & 
& nodecnc=[293,1843,1765,1921,2865,4415,4337,4493,23057, 23058,7512, 7511,23059, 23060,23061, 23062,23063 & 
& , 23064,7520, 7519,23065, 23066,23067, 23068], & 
& edgecnc=[8957,1184,8958,8959,8960,1188,8961,8962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1951),elname="xbrick",eltype="xbrick",typekey=1951) 

        call prepare(lib_xbrick(1952),key=1952, & 
& nodecnc=[125,1921,1765,1828,2697,4493,4337,4400,23069, 23070,23060, 23059,10450, 10449,23071, 23072 & 
& ,23073, 23074,23066, 23065,10454, 10453,23075, 23076], & 
& edgecnc=[8963,8958,2653,8964,8965,8961,2655,8966], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1952),elname="xbrick",eltype="xbrick",typekey=1952) 

        call prepare(lib_xbrick(1953),key=1953, & 
& nodecnc=[2360,1829,1766,1909,4932,4401,4338,4481,22798, 22797,12620, 12619,23077, 23078,23079, 23080 & 
& ,22804, 22803,12624, 12623,23081, 23082,23083, 23084], & 
& edgecnc=[8827,3738,8967,8968,8830,3740,8969,8970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1953),elname="xbrick",eltype="xbrick",typekey=1953) 

        call prepare(lib_xbrick(1954),key=1954, & 
& nodecnc=[293,1909,1766,1844,2865,4481,4338,4416,23085, 23086,23078, 23077,6942, 6941,23087, 23088,23089 & 
& , 23090,23082, 23081,6950, 6949,23091, 23092], & 
& edgecnc=[8971,8967,899,8972,8973,8969,903,8974], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1954),elname="xbrick",eltype="xbrick",typekey=1954) 

        call prepare(lib_xbrick(1955),key=1955, & 
& nodecnc=[2357,1831,1767,1910,4929,4403,4339,4482,6070, 6069,12590, 12589,23093, 23094,23095, 23096,6078 & 
& , 6077,12596, 12595,23097, 23098,23099, 23100], & 
& edgecnc=[463,3723,8975,8976,467,3726,8977,8978], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1955),elname="xbrick",eltype="xbrick",typekey=1955) 

        call prepare(lib_xbrick(1956),key=1956, & 
& nodecnc=[290,1910,1767,1845,2862,4482,4339,4417,23101, 23102,23094, 23093,6058, 6057,23103, 23104,23105 & 
& , 23106,23098, 23097,6064, 6063,23107, 23108], & 
& edgecnc=[8979,8975,457,8980,8981,8977,460,8982], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1956),elname="xbrick",eltype="xbrick",typekey=1956) 

        call prepare(lib_xbrick(1957),key=1957, & 
& nodecnc=[290,1846,1768,1920,2862,4418,4340,4492,23109, 23110,12600, 12599,23111, 23112,23113, 23114 & 
& ,23115, 23116,12604, 12603,23117, 23118,23119, 23120], & 
& edgecnc=[8983,3728,8984,8985,8986,3730,8987,8988], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1957),elname="xbrick",eltype="xbrick",typekey=1957) 

        call prepare(lib_xbrick(1958),key=1958, & 
& nodecnc=[192,1920,1768,1830,2764,4492,4340,4402,12576, 12575,23112, 23111,6154, 6153,23121, 23122,12582 & 
& , 12581,23118, 23117,6162, 6161,23123, 23124], & 
& edgecnc=[3716,8984,505,8989,3719,8987,509,8990], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1958),elname="xbrick",eltype="xbrick",typekey=1958) 

        call prepare(lib_xbrick(1959),key=1959, & 
& nodecnc=[1817,2516,1934,1769,4389,5088,4506,4341,23125, 23126,23127, 23128,22550, 22549,23129, 23130 & 
& ,23131, 23132,23133, 23134,22552, 22551,23135, 23136], & 
& edgecnc=[8991,8992,8703,8993,8994,8995,8704,8996], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1959),elname="xbrick",eltype="xbrick",typekey=1959) 

        call prepare(lib_xbrick(1960),key=1960, & 
& nodecnc=[1817,1769,1786,115,4389,4341,4358,2687,23130, 23129,22384, 22383,9222, 9221,23137, 23138,23136 & 
& , 23135,22388, 22387,9230, 9229,23139, 23140], & 
& edgecnc=[8993,8620,2039,8997,8996,8622,2043,8998], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1960),elname="xbrick",eltype="xbrick",typekey=1960) 

        call prepare(lib_xbrick(1961),key=1961, & 
& nodecnc=[1995,1873,1832,1770,4567,4445,4404,4342,23141, 23142,10142, 10141,10114, 10113,23143, 23144 & 
& ,23145, 23146,10148, 10147,10120, 10119,23147, 23148], & 
& edgecnc=[8999,2499,2485,9000,9001,2502,2488,9002], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1961),elname="xbrick",eltype="xbrick",typekey=1961) 

        call prepare(lib_xbrick(1962),key=1962, & 
& nodecnc=[279,1794,1818,1862,2851,4366,4390,4434,22350, 22349,23149, 23150,23151, 23152,9946, 9945,22354 & 
& , 22353,23153, 23154,23155, 23156,9954, 9953], & 
& edgecnc=[8603,9003,9004,2401,8605,9005,9006,2405], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1962),elname="xbrick",eltype="xbrick",typekey=1962) 

        call prepare(lib_xbrick(1963),key=1963, & 
& nodecnc=[1862,1818,1774,245,4434,4390,4346,2817,23152, 23151,22786, 22785,23157, 23158,23159, 23160 & 
& ,23156, 23155,22790, 22789,23161, 23162,23163, 23164], & 
& edgecnc=[9004,8821,9007,9008,9006,8823,9009,9010], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1963),elname="xbrick",eltype="xbrick",typekey=1963) 

        call prepare(lib_xbrick(1964),key=1964, & 
& nodecnc=[300,1903,1777,1848,2872,4475,4349,4420,23165, 23166,23167, 23168,8178, 8177,23169, 23170,23171 & 
& , 23172,23173, 23174,8186, 8185,23175, 23176], & 
& edgecnc=[9011,9012,1517,9013,9014,9015,1521,9016], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1964),elname="xbrick",eltype="xbrick",typekey=1964) 

        call prepare(lib_xbrick(1965),key=1965, & 
& nodecnc=[281,1835,1778,1971,2853,4407,4350,4543,22958, 22957,8972, 8971,23177, 23178,23179, 23180,22960 & 
& , 22959,8976, 8975,23181, 23182,23183, 23184], & 
& edgecnc=[8907,1914,9017,9018,8908,1916,9019,9020], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1965),elname="xbrick",eltype="xbrick",typekey=1965) 

        call prepare(lib_xbrick(1966),key=1966, & 
& nodecnc=[1971,1778,1849,1904,4543,4350,4421,4476,23178, 23177,8954, 8953,23185, 23186,23187, 23188,23182 & 
& , 23181,8962, 8961,23189, 23190,23191, 23192], & 
& edgecnc=[9017,1905,9021,9022,9019,1909,9023,9024], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1966),elname="xbrick",eltype="xbrick",typekey=1966) 

        call prepare(lib_xbrick(1967),key=1967, & 
& nodecnc=[298,1851,1779,2000,2870,4423,4351,4572,23193, 23194,12666, 12665,23195, 23196,23197, 23198 & 
& ,23199, 23200,12668, 12667,23201, 23202,23203, 23204], & 
& edgecnc=[9025,3761,9026,9027,9028,3762,9029,9030], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1967),elname="xbrick",eltype="xbrick",typekey=1967) 

        call prepare(lib_xbrick(1968),key=1968, & 
& nodecnc=[284,1852,1780,1999,2856,4424,4352,4571,23205, 23206,10270, 10269,23207, 23208,23209, 23210 & 
& ,23211, 23212,10276, 10275,23213, 23214,23215, 23216], & 
& edgecnc=[9031,2563,9032,9033,9034,2566,9035,9036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1968),elname="xbrick",eltype="xbrick",typekey=1968) 

        call prepare(lib_xbrick(1969),key=1969, & 
& nodecnc=[323,1841,1781,1839,2895,4413,4353,4411,23024, 23023,22626, 22625,5862, 5861,22998, 22997,23028 & 
& , 23027,22628, 22627,5870, 5869,23004, 23003], & 
& edgecnc=[8940,8741,359,8927,8942,8742,363,8930], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1969),elname="xbrick",eltype="xbrick",typekey=1969) 

        call prepare(lib_xbrick(1970),key=1970, & 
& nodecnc=[259,1842,1782,1840,2831,4414,4354,4412,23052, 23051,22622, 22621,23217, 23218,23030, 23029 & 
& ,23056, 23055,22624, 22623,23219, 23220,23036, 23035], & 
& edgecnc=[8954,8739,9037,8943,8956,8740,9038,8946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1970),elname="xbrick",eltype="xbrick",typekey=1970) 

        call prepare(lib_xbrick(1971),key=1971, & 
& nodecnc=[293,1844,1783,1843,2865,4416,4355,4415,23088, 23087,7526, 7525,23221, 23222,23058, 23057,23092 & 
& , 23091,7532, 7531,23223, 23224,23064, 23063], & 
& edgecnc=[8972,1191,9039,8957,8974,1194,9040,8960], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1971),elname="xbrick",eltype="xbrick",typekey=1971) 

        call prepare(lib_xbrick(1972),key=1972, & 
& nodecnc=[290,1845,1784,1846,2862,4417,4356,4418,23104, 23103,6048, 6047,23225, 23226,23110, 23109,23108 & 
& , 23107,6056, 6055,23227, 23228,23116, 23115], & 
& edgecnc=[8980,452,9041,8983,8982,456,9042,8986], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1972),elname="xbrick",eltype="xbrick",typekey=1972) 

        call prepare(lib_xbrick(1973),key=1973, & 
& nodecnc=[1875,1833,1785,2496,4447,4405,4357,5068,23229, 23230,10058, 10057,23231, 23232,22724, 22723 & 
& ,23233, 23234,10064, 10063,23235, 23236,22732, 22731], & 
& edgecnc=[9043,2457,9044,8790,9045,2460,9046,8794], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1973),elname="xbrick",eltype="xbrick",typekey=1973) 

        call prepare(lib_xbrick(1974),key=1974, & 
& nodecnc=[1816,2496,1785,1807,4388,5068,4357,4379,23237, 23238,23232, 23231,21672, 21671,23239, 23240 & 
& ,23241, 23242,23236, 23235,21678, 21677,23243, 23244], & 
& edgecnc=[9047,9044,8264,9048,9049,9046,8267,9050], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1974),elname="xbrick",eltype="xbrick",typekey=1974) 

        call prepare(lib_xbrick(1975),key=1975, & 
& nodecnc=[1787,1810,266,1876,4359,4382,2838,4448,12210, 12209,23245, 23246,23247, 23248,23249, 23250 & 
& ,12214, 12213,23251, 23252,23253, 23254,23255, 23256], & 
& edgecnc=[3533,9051,9052,9053,3535,9054,9055,9056], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1975),elname="xbrick",eltype="xbrick",typekey=1975) 

        call prepare(lib_xbrick(1976),key=1976, & 
& nodecnc=[266,1810,1788,1795,2838,4382,4360,4367,23246, 23245,22618, 22617,12262, 12261,22688, 22687 & 
& ,23252, 23251,22620, 22619,12268, 12267,22692, 22691], & 
& edgecnc=[9051,8737,3559,8772,9054,8738,3562,8774], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1976),elname="xbrick",eltype="xbrick",typekey=1976) 

        call prepare(lib_xbrick(1977),key=1977, & 
& nodecnc=[299,1855,1789,1880,2871,4427,4361,4452,23257, 23258,12672, 12671,23259, 23260,23261, 23262 & 
& ,23263, 23264,12676, 12675,23265, 23266,23267, 23268], & 
& edgecnc=[9057,3764,9058,9059,9060,3766,9061,9062], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1977),elname="xbrick",eltype="xbrick",typekey=1977) 

        call prepare(lib_xbrick(1978),key=1978, & 
& nodecnc=[1851,1990,1880,1789,4423,4562,4452,4361,23269, 23270,23271, 23272,23260, 23259,10714, 10713 & 
& ,23273, 23274,23275, 23276,23266, 23265,10722, 10721], & 
& edgecnc=[9063,9064,9058,2785,9065,9066,9061,2789], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1978),elname="xbrick",eltype="xbrick",typekey=1978) 

        call prepare(lib_xbrick(1979),key=1979, & 
& nodecnc=[283,1856,1790,1881,2855,4428,4362,4453,23277, 23278,10290, 10289,23279, 23280,23281, 23282 & 
& ,23283, 23284,10296, 10295,23285, 23286,23287, 23288], & 
& edgecnc=[9067,2573,9068,9069,9070,2576,9071,9072], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1979),elname="xbrick",eltype="xbrick",typekey=1979) 

        call prepare(lib_xbrick(1980),key=1980, & 
& nodecnc=[1852,1989,1881,1790,4424,4561,4453,4362,23289, 23290,23291, 23292,23280, 23279,10278, 10277 & 
& ,23293, 23294,23295, 23296,23286, 23285,10282, 10281], & 
& edgecnc=[9073,9074,9068,2567,9075,9076,9071,2569], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1980),elname="xbrick",eltype="xbrick",typekey=1980) 

        call prepare(lib_xbrick(1981),key=1981, & 
& nodecnc=[300,1848,1791,1878,2872,4420,4363,4450,23170, 23169,12690, 12689,23297, 23298,23299, 23300 & 
& ,23176, 23175,12692, 12691,23301, 23302,23303, 23304], & 
& edgecnc=[9013,3773,9077,9078,9016,3774,9079,9080], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1981),elname="xbrick",eltype="xbrick",typekey=1981) 

        call prepare(lib_xbrick(1982),key=1982, & 
& nodecnc=[299,1878,1791,1855,2871,4450,4363,4427,23305, 23306,23298, 23297,12678, 12677,23258, 23257 & 
& ,23307, 23308,23302, 23301,12684, 12683,23264, 23263], & 
& edgecnc=[9081,9077,3767,9057,9082,9079,3770,9060], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1982),elname="xbrick",eltype="xbrick",typekey=1982) 

        call prepare(lib_xbrick(1983),key=1983, & 
& nodecnc=[282,1849,1792,1879,2854,4421,4364,4451,23309, 23310,9258, 9257,23311, 23312,23313, 23314,23315 & 
& , 23316,9264, 9263,23317, 23318,23319, 23320], & 
& edgecnc=[9083,2057,9084,9085,9086,2060,9087,9088], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1983),elname="xbrick",eltype="xbrick",typekey=1983) 

        call prepare(lib_xbrick(1984),key=1984, & 
& nodecnc=[283,1879,1792,1856,2855,4451,4364,4428,23321, 23322,23312, 23311,12394, 12393,23278, 23277 & 
& ,23323, 23324,23318, 23317,12396, 12395,23284, 23283], & 
& edgecnc=[9089,9084,3625,9067,9090,9087,3626,9070], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1984),elname="xbrick",eltype="xbrick",typekey=1984) 

        call prepare(lib_xbrick(1985),key=1985, & 
& nodecnc=[1793,1853,1805,216,4365,4425,4377,2788,7234, 7233,12482, 12481,22674, 22673,22770, 22769,7240 & 
& , 7239,12488, 12487,22676, 22675,22772, 22771], & 
& edgecnc=[1045,3669,8765,8813,1048,3672,8766,8814], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1985),elname="xbrick",eltype="xbrick",typekey=1985) 

        call prepare(lib_xbrick(1986),key=1986, & 
& nodecnc=[278,1819,1818,1794,2850,4391,4390,4366,23325, 23326,22788, 22787,23150, 23149,12520, 12519 & 
& ,23327, 23328,22792, 22791,23154, 23153,12526, 12525], & 
& edgecnc=[9091,8822,9003,3688,9092,8824,9005,3691], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1986),elname="xbrick",eltype="xbrick",typekey=1986) 

        call prepare(lib_xbrick(1987),key=1987, & 
& nodecnc=[1798,1796,1701,336,4370,4368,4273,2908,6732, 6731,22678, 22677,7156, 7155,8206, 8205,6738, 6737 & 
& ,22680, 22679,7164, 7163,8212, 8211], & 
& edgecnc=[794,8767,1006,1531,797,8768,1010,1534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1987),elname="xbrick",eltype="xbrick",typekey=1987) 

        call prepare(lib_xbrick(1988),key=1988, & 
& nodecnc=[1820,246,1799,2279,4392,2818,4371,4851,23329, 23330,22050, 22049,12544, 12543,22740, 22739 & 
& ,23331, 23332,22054, 22053,12548, 12547,22746, 22745], & 
& edgecnc=[9093,8453,3700,8798,9094,8455,3702,8801], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1988),elname="xbrick",eltype="xbrick",typekey=1988) 

        call prepare(lib_xbrick(1989),key=1989, & 
& nodecnc=[2261,353,1801,2263,4833,2925,4373,4835,22870, 22869,21962, 21961,23333, 23334,12870, 12869 & 
& ,22876, 22875,21964, 21963,23335, 23336,12878, 12877], & 
& edgecnc=[8863,8409,9095,3863,8866,8410,9096,3867], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1989),elname="xbrick",eltype="xbrick",typekey=1989) 

        call prepare(lib_xbrick(1990),key=1990, & 
& nodecnc=[1811,1802,1493,1471,4383,4374,4065,4043,21902, 21901,13758, 13757,21890, 21889,8568, 8567,21906 & 
& , 21905,13764, 13763,21896, 21895,8572, 8571], & 
& edgecnc=[8379,4307,8373,1712,8381,4310,8376,1714], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1990),elname="xbrick",eltype="xbrick",typekey=1990) 

        call prepare(lib_xbrick(1991),key=1991, & 
& nodecnc=[2263,1801,1719,1803,4835,4373,4291,4375,23334, 23333,13062, 13061,22430, 22429,12872, 12871 & 
& ,23336, 23335,13068, 13067,22432, 22431,12880, 12879], & 
& edgecnc=[9095,3959,8643,3864,9096,3962,8644,3868], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1991),elname="xbrick",eltype="xbrick",typekey=1991) 

        call prepare(lib_xbrick(1992),key=1992, & 
& nodecnc=[1935,265,1876,1970,4507,2837,4448,4542,23337, 23338,23339, 23340,23341, 23342,23343, 23344 & 
& ,23345, 23346,23347, 23348,23349, 23350,23351, 23352], & 
& edgecnc=[9097,9098,9099,9100,9101,9102,9103,9104], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1992),elname="xbrick",eltype="xbrick",typekey=1992) 

        call prepare(lib_xbrick(1993),key=1993, & 
& nodecnc=[1816,1807,1432,2459,4388,4379,4004,5031,23240, 23239,21730, 21729,23353, 23354,10072, 10071 & 
& ,23244, 23243,21732, 21731,23355, 23356,10080, 10079], & 
& edgecnc=[9048,8293,9105,2464,9050,8294,9106,2468], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1993),elname="xbrick",eltype="xbrick",typekey=1993) 

        call prepare(lib_xbrick(1994),key=1994, & 
& nodecnc=[2001,1888,1939,153,4573,4460,4511,2725,23357, 23358,11410, 11409,23359, 23360,23361, 23362 & 
& ,23363, 23364,11416, 11415,23365, 23366,23367, 23368], & 
& edgecnc=[9107,3133,9108,9109,9110,3136,9111,9112], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1994),elname="xbrick",eltype="xbrick",typekey=1994) 

        call prepare(lib_xbrick(1995),key=1995, & 
& nodecnc=[265,1850,1787,1876,2837,4422,4359,4448,23369, 23370,7304, 7303,23250, 23249,23340, 23339,23371 & 
& , 23372,7312, 7311,23256, 23255,23348, 23347], & 
& edgecnc=[9113,1080,9053,9098,9114,1084,9056,9102], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1995),elname="xbrick",eltype="xbrick",typekey=1995) 

        call prepare(lib_xbrick(1996),key=1996, & 
& nodecnc=[1914,1812,1615,279,4486,4384,4187,2851,23373, 23374,9936, 9935,22352, 22351,9952, 9951,23375 & 
& , 23376,9944, 9943,22356, 22355,9960, 9959], & 
& edgecnc=[9115,2396,8604,2404,9116,2400,8606,2408], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1996),elname="xbrick",eltype="xbrick",typekey=1996) 

        call prepare(lib_xbrick(1997),key=1997, & 
& nodecnc=[1929,280,2295,1987,4501,2852,4867,4559,23377, 23378,22884, 22883,22952, 22951,5690, 5689,23379 & 
& , 23380,22890, 22889,22956, 22955,5698, 5697], & 
& edgecnc=[9117,8870,8904,273,9118,8873,8906,277], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1997),elname="xbrick",eltype="xbrick",typekey=1997) 

        call prepare(lib_xbrick(1998),key=1998, & 
& nodecnc=[2516,114,1813,1934,5088,2686,4385,4506,23381, 23382,23383, 23384,22542, 22541,23128, 23127 & 
& ,23385, 23386,23387, 23388,22548, 22547,23134, 23133], & 
& edgecnc=[9119,9120,8699,8992,9121,9122,8702,8995], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1998),elname="xbrick",eltype="xbrick",typekey=1998) 

        call prepare(lib_xbrick(1999),key=1999, & 
& nodecnc=[1771,1732,1814,1867,4343,4304,4386,4439,22902, 22901,19944, 19943,23389, 23390,19584, 19583 & 
& ,22904, 22903,19948, 19947,23391, 23392,19590, 19589], & 
& edgecnc=[8879,7400,9123,7220,8880,7402,9124,7223], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(1999),elname="xbrick",eltype="xbrick",typekey=1999) 

        call prepare(lib_xbrick(2000),key=2000, & 
& nodecnc=[1867,1814,1819,278,4439,4386,4391,2850,23390, 23389,20122, 20121,23326, 23325,19586, 19585 & 
& ,23392, 23391,20126, 20125,23328, 23327,19592, 19591], & 
& edgecnc=[9123,7489,9091,7221,9124,7491,9092,7224], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2000),elname="xbrick",eltype="xbrick",typekey=2000) 

        call prepare(lib_xbrick(2001),key=2001, & 
& nodecnc=[2496,1816,1915,1931,5068,4388,4487,4503,23238, 23237,10070, 10069,10002, 10001,22726, 22725 & 
& ,23242, 23241,10078, 10077,10010, 10009,22734, 22733], & 
& edgecnc=[9047,2463,2429,8791,9049,2467,2433,8795], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2001),elname="xbrick",eltype="xbrick",typekey=2001) 

        call prepare(lib_xbrick(2002),key=2002, & 
& nodecnc=[1962,2504,1998,2526,4534,5076,4570,5098,8980, 8979,23393, 23394,23395, 23396,23397, 23398,8988 & 
& , 8987,23399, 23400,23401, 23402,23403, 23404], & 
& edgecnc=[1918,9125,9126,9127,1922,9128,9129,9130], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2002),elname="xbrick",eltype="xbrick",typekey=2002) 

        call prepare(lib_xbrick(2003),key=2003, & 
& nodecnc=[2295,1746,246,1820,4867,4318,2818,4392,22882, 22881,22646, 22645,23330, 23329,22950, 22949 & 
& ,22888, 22887,22650, 22649,23332, 23331,22954, 22953], & 
& edgecnc=[8869,8751,9093,8903,8872,8753,9094,8905], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2003),elname="xbrick",eltype="xbrick",typekey=2003) 

        call prepare(lib_xbrick(2004),key=2004, & 
& nodecnc=[1952,286,2356,1891,4524,2858,4928,4463,23405, 23406,23407, 23408,5968, 5967,11968, 11967,23409 & 
& , 23410,23411, 23412,5976, 5975,11976, 11975], & 
& edgecnc=[9131,9132,412,3412,9133,9134,416,3416], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2004),elname="xbrick",eltype="xbrick",typekey=2004) 

        call prepare(lib_xbrick(2005),key=2005, & 
& nodecnc=[1737,1898,125,1828,4309,4470,2697,4400,11798, 11797,23413, 23414,23072, 23071,22810, 22809 & 
& ,11802, 11801,23415, 23416,23076, 23075,22812, 22811], & 
& edgecnc=[3327,9135,8964,8833,3329,9136,8966,8834], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2005),elname="xbrick",eltype="xbrick",typekey=2005) 

        call prepare(lib_xbrick(2006),key=2006, & 
& nodecnc=[1735,1900,192,1830,4307,4472,2764,4402,9654, 9653,23417, 23418,23122, 23121,22794, 22793,9662 & 
& , 9661,23419, 23420,23124, 23123,22796, 22795], & 
& edgecnc=[2255,9137,8989,8825,2259,9138,8990,8826], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2006),elname="xbrick",eltype="xbrick",typekey=2006) 

        call prepare(lib_xbrick(2007),key=2007, & 
& nodecnc=[1877,1986,1886,183,4449,4558,4458,2755,7978, 7977,23421, 23422,23423, 23424,7966, 7965,7984 & 
& , 7983,23425, 23426,23427, 23428,7974, 7973], & 
& edgecnc=[1417,9139,9140,1411,1420,9141,9142,1415], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2007),elname="xbrick",eltype="xbrick",typekey=2007) 

        call prepare(lib_xbrick(2008),key=2008, & 
& nodecnc=[1875,2534,116,1833,4447,5106,2688,4405,23429, 23430,10322, 10321,9314, 9313,23230, 23229,23431 & 
& , 23432,10328, 10327,9320, 9319,23234, 23233], & 
& edgecnc=[9143,2589,2085,9043,9144,2592,2088,9045], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2008),elname="xbrick",eltype="xbrick",typekey=2008) 

        call prepare(lib_xbrick(2009),key=2009, & 
& nodecnc=[301,1834,1777,1903,2873,4406,4349,4475,22942, 22941,8198, 8197,23168, 23167,23433, 23434,22948 & 
& , 22947,8204, 8203,23174, 23173,23435, 23436], & 
& edgecnc=[8899,1527,9012,9145,8902,1530,9015,9146], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2009),elname="xbrick",eltype="xbrick",typekey=2009) 

        call prepare(lib_xbrick(2010),key=2010, & 
& nodecnc=[265,1973,1776,1850,2837,4545,4348,4422,23437, 23438,22762, 22761,7330, 7329,23370, 23369,23439 & 
& , 23440,22768, 22767,7334, 7333,23372, 23371], & 
& edgecnc=[9147,8809,1093,9113,9148,8812,1095,9114], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2010),elname="xbrick",eltype="xbrick",typekey=2010) 

        call prepare(lib_xbrick(2011),key=2011, & 
& nodecnc=[1906,2418,285,2380,4478,4990,2857,4952,23441, 23442,23443, 23444,23445, 23446,22990, 22989 & 
& ,23447, 23448,23449, 23450,23451, 23452,22994, 22993], & 
& edgecnc=[9149,9150,9151,8923,9152,9153,9154,8925], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2011),elname="xbrick",eltype="xbrick",typekey=2011) 

        call prepare(lib_xbrick(2012),key=2012, & 
& nodecnc=[2239,1928,1888,2001,4811,4500,4460,4573,23453, 23454,9100, 9099,23358, 23357,23455, 23456,23457 & 
& , 23458,9108, 9107,23364, 23363,23459, 23460], & 
& edgecnc=[9155,1978,9107,9156,9157,1982,9110,9158], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2012),elname="xbrick",eltype="xbrick",typekey=2012) 

        call prepare(lib_xbrick(2013),key=2013, & 
& nodecnc=[1702,152,1847,2336,4274,2724,4419,4908,22514, 22513,9104, 9103,22558, 22557,11436, 11435,22516 & 
& , 22515,9112, 9111,22560, 22559,11440, 11439], & 
& edgecnc=[8685,1980,8707,3146,8686,1984,8708,3148], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2013),elname="xbrick",eltype="xbrick",typekey=2013) 

        call prepare(lib_xbrick(2014),key=2014, & 
& nodecnc=[1916,1970,1917,1853,4488,4542,4489,4425,23461, 23462,23463, 23464,12478, 12477,7232, 7231,23465 & 
& , 23466,23467, 23468,12484, 12483,7238, 7237], & 
& edgecnc=[9159,9160,3667,1044,9161,9162,3670,1047], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2014),elname="xbrick",eltype="xbrick",typekey=2014) 

        call prepare(lib_xbrick(2015),key=2015, & 
& nodecnc=[1886,1854,1809,183,4458,4426,4381,2755,6774, 6773,7222, 7221,12126, 12125,23424, 23423,6782 & 
& , 6781,7228, 7227,12130, 12129,23428, 23427], & 
& edgecnc=[815,1039,3491,9140,819,1042,3493,9142], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2015),elname="xbrick",eltype="xbrick",typekey=2015) 

        call prepare(lib_xbrick(2016),key=2016, & 
& nodecnc=[2019,2383,1916,217,4591,4955,4488,2789,23469, 23470,23471, 23472,7230, 7229,6792, 6791,23473 & 
& , 23474,23475, 23476,7236, 7235,6798, 6797], & 
& edgecnc=[9163,9164,1043,824,9165,9166,1046,827], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2016),elname="xbrick",eltype="xbrick",typekey=2016) 

        call prepare(lib_xbrick(2017),key=2017, & 
& nodecnc=[2471,2025,1938,2519,5043,4597,4510,5091,23477, 23478,23479, 23480,23481, 23482,23483, 23484 & 
& ,23485, 23486,23487, 23488,23489, 23490,23491, 23492], & 
& edgecnc=[9167,9168,9169,9170,9171,9172,9173,9174], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2017),elname="xbrick",eltype="xbrick",typekey=2017) 

        call prepare(lib_xbrick(2018),key=2018, & 
& nodecnc=[1969,1939,1808,1859,4541,4511,4380,4431,23493, 23494,11408, 11407,9000, 8999,23495, 23496,23497 & 
& , 23498,11414, 11413,9008, 9007,23499, 23500], & 
& edgecnc=[9175,3132,1928,9176,9177,3135,1932,9178], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2018),elname="xbrick",eltype="xbrick",typekey=2018) 

        call prepare(lib_xbrick(2019),key=2019, & 
& nodecnc=[2462,1863,1929,1983,5034,4435,4501,4555,11998, 11997,23501, 23502,23503, 23504,23505, 23506 & 
& ,12004, 12003,23507, 23508,23509, 23510,23511, 23512], & 
& edgecnc=[3427,9179,9180,9181,3430,9182,9183,9184], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2019),elname="xbrick",eltype="xbrick",typekey=2019) 

        call prepare(lib_xbrick(2020),key=2020, & 
& nodecnc=[1929,1863,2385,280,4501,4435,4957,2852,23502, 23501,22776, 22775,23513, 23514,23378, 23377 & 
& ,23508, 23507,22780, 22779,23515, 23516,23380, 23379], & 
& edgecnc=[9179,8816,9185,9117,9182,8818,9186,9118], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2020),elname="xbrick",eltype="xbrick",typekey=2020) 

        call prepare(lib_xbrick(2021),key=2021, & 
& nodecnc=[1846,1784,1685,291,4418,4356,4257,2863,23226, 23225,22642, 22641,22150, 22149,12598, 12597 & 
& ,23228, 23227,22644, 22643,22152, 22151,12602, 12601], & 
& edgecnc=[9041,8749,8503,3727,9042,8750,8504,3729], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2021),elname="xbrick",eltype="xbrick",typekey=2021) 

        call prepare(lib_xbrick(2022),key=2022, & 
& nodecnc=[1843,1783,1684,292,4415,4355,4256,2864,23222, 23221,22638, 22637,7810, 7809,7506, 7505,23224 & 
& , 23223,22640, 22639,7818, 7817,7514, 7513], & 
& edgecnc=[9039,8747,1333,1181,9040,8748,1337,1185], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2022),elname="xbrick",eltype="xbrick",typekey=2022) 

        call prepare(lib_xbrick(2023),key=2023, & 
& nodecnc=[1840,1782,1696,225,4412,4354,4268,2797,23218, 23217,7628, 7627,22286, 22285,7426, 7425,23220 & 
& , 23219,7634, 7633,22288, 22287,7434, 7433], & 
& edgecnc=[9037,1242,8571,1141,9038,1245,8572,1145], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2023),elname="xbrick",eltype="xbrick",typekey=2023) 

        call prepare(lib_xbrick(2024),key=2024, & 
& nodecnc=[1817,115,1864,1883,4389,2687,4436,4455,23138, 23137,9228, 9227,23517, 23518,12176, 12175,23140 & 
& , 23139,9236, 9235,23519, 23520,12182, 12181], & 
& edgecnc=[8997,2042,9187,3516,8998,2046,9188,3519], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2024),elname="xbrick",eltype="xbrick",typekey=2024) 

        call prepare(lib_xbrick(2025),key=2025, & 
& nodecnc=[1919,1982,1883,1864,4491,4554,4455,4436,10240, 10239,23521, 23522,23518, 23517,10326, 10325 & 
& ,10248, 10247,23523, 23524,23520, 23519,10332, 10331], & 
& edgecnc=[2548,9189,9187,2591,2552,9190,9188,2594], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2025),elname="xbrick",eltype="xbrick",typekey=2025) 

        call prepare(lib_xbrick(2026),key=2026, & 
& nodecnc=[2261,1985,1872,1745,4833,4557,4444,4317,22894, 22893,9354, 9353,23525, 23526,22866, 22865,22898 & 
& , 22897,9362, 9361,23527, 23528,22872, 22871], & 
& edgecnc=[8875,2105,9191,8861,8877,2109,9192,8864], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2026),elname="xbrick",eltype="xbrick",typekey=2026) 

        call prepare(lib_xbrick(2027),key=2027, & 
& nodecnc=[2491,1981,2164,2514,5063,4553,4736,5086,5674, 5673,23529, 23530,23531, 23532,23533, 23534,5682 & 
& , 5681,23535, 23536,23537, 23538,23539, 23540], & 
& edgecnc=[265,9193,9194,9195,269,9196,9197,9198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2027),elname="xbrick",eltype="xbrick",typekey=2027) 

        call prepare(lib_xbrick(2028),key=2028, & 
& nodecnc=[130,1974,1866,1918,2702,4546,4438,4490,23541, 23542,11712, 11711,23543, 23544,23545, 23546 & 
& ,23547, 23548,11720, 11719,23549, 23550,23551, 23552], & 
& edgecnc=[9199,3284,9200,9201,9202,3288,9203,9204], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2028),elname="xbrick",eltype="xbrick",typekey=2028) 

        call prepare(lib_xbrick(2029),key=2029, & 
& nodecnc=[1918,1866,1755,131,4490,4438,4327,2703,23544, 23543,22500, 22499,22932, 22931,10784, 10783 & 
& ,23550, 23549,22504, 22503,22936, 22935,10792, 10791], & 
& edgecnc=[9200,8678,8894,2820,9203,8680,8896,2824], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2029),elname="xbrick",eltype="xbrick",typekey=2029) 

        call prepare(lib_xbrick(2030),key=2030, & 
& nodecnc=[1912,2276,1924,2098,4484,4848,4496,4670,23553, 23554,12944, 12943,23555, 23556,22226, 22225 & 
& ,23557, 23558,12948, 12947,23559, 23560,22228, 22227], & 
& edgecnc=[9205,3900,9206,8541,9207,3902,9208,8542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2030),elname="xbrick",eltype="xbrick",typekey=2030) 

        call prepare(lib_xbrick(2031),key=2031, & 
& nodecnc=[1927,2101,1911,2277,4499,4673,4483,4849,23561, 23562,22158, 22157,23563, 23564,6092, 6091,23565 & 
& , 23566,22160, 22159,23567, 23568,6100, 6099], & 
& edgecnc=[9209,8507,9210,474,9211,8508,9212,478], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2031),elname="xbrick",eltype="xbrick",typekey=2031) 

        call prepare(lib_xbrick(2032),key=2032, & 
& nodecnc=[1873,1995,1988,113,4445,4567,4560,2685,23142, 23141,10224, 10223,23569, 23570,23571, 23572 & 
& ,23146, 23145,10232, 10231,23573, 23574,23575, 23576], & 
& edgecnc=[8999,2540,9213,9214,9001,2544,9215,9216], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2032),elname="xbrick",eltype="xbrick",typekey=2032) 

        call prepare(lib_xbrick(2033),key=2033, & 
& nodecnc=[2056,1874,1873,113,4628,4446,4445,2685,5816, 5815,10138, 10137,23572, 23571,10206, 10205,5824 & 
& , 5823,10144, 10143,23576, 23575,10212, 10211], & 
& edgecnc=[336,2497,9214,2531,340,2500,9216,2534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2033),elname="xbrick",eltype="xbrick",typekey=2033) 

        call prepare(lib_xbrick(2034),key=2034, & 
& nodecnc=[1996,2003,2534,1875,4568,4575,5106,4447,23577, 23578,23579, 23580,23430, 23429,22722, 22721 & 
& ,23581, 23582,23583, 23584,23432, 23431,22730, 22729], & 
& edgecnc=[9217,9218,9143,8789,9219,9220,9144,8793], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2034),elname="xbrick",eltype="xbrick",typekey=2034) 

        call prepare(lib_xbrick(2035),key=2035, & 
& nodecnc=[266,1917,1970,1876,2838,4489,4542,4448,22686, 22685,23464, 23463,23342, 23341,23248, 23247 & 
& ,22690, 22689,23468, 23467,23350, 23349,23254, 23253], & 
& edgecnc=[8771,9160,9099,9052,8773,9162,9103,9055], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2035),elname="xbrick",eltype="xbrick",typekey=2035) 

        call prepare(lib_xbrick(2036),key=2036, & 
& nodecnc=[299,1960,2465,1878,2871,4532,5037,4450,23585, 23586,10762, 10761,23587, 23588,23306, 23305 & 
& ,23589, 23590,10770, 10769,23591, 23592,23308, 23307], & 
& edgecnc=[9221,2809,9222,9081,9223,2813,9224,9082], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2036),elname="xbrick",eltype="xbrick",typekey=2036) 

        call prepare(lib_xbrick(2037),key=2037, & 
& nodecnc=[1960,299,1880,2436,4532,2871,4452,5008,23586, 23585,23262, 23261,23593, 23594,23595, 23596 & 
& ,23590, 23589,23268, 23267,23597, 23598,23599, 23600], & 
& edgecnc=[9221,9059,9225,9226,9223,9062,9227,9228], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2037),elname="xbrick",eltype="xbrick",typekey=2037) 

        call prepare(lib_xbrick(2038),key=2038, & 
& nodecnc=[1956,2436,1880,1990,4528,5008,4452,4562,10702, 10701,23594, 23593,23272, 23271,23601, 23602 & 
& ,10710, 10709,23598, 23597,23276, 23275,23603, 23604], & 
& edgecnc=[2779,9225,9064,9229,2783,9227,9066,9230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2038),elname="xbrick",eltype="xbrick",typekey=2038) 

        call prepare(lib_xbrick(2039),key=2039, & 
& nodecnc=[2467,283,1881,1955,5039,2855,4453,4527,23605, 23606,23282, 23281,23607, 23608,23609, 23610 & 
& ,23611, 23612,23288, 23287,23613, 23614,23615, 23616], & 
& edgecnc=[9231,9069,9232,9233,9234,9072,9235,9236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2039),elname="xbrick",eltype="xbrick",typekey=2039) 

        call prepare(lib_xbrick(2040),key=2040, & 
& nodecnc=[2154,1955,1881,1989,4726,4527,4453,4561,23617, 23618,23608, 23607,23292, 23291,23619, 23620 & 
& ,23621, 23622,23614, 23613,23296, 23295,23623, 23624], & 
& edgecnc=[9237,9232,9074,9238,9239,9235,9076,9240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2040),elname="xbrick",eltype="xbrick",typekey=2040) 

        call prepare(lib_xbrick(2041),key=2041, & 
& nodecnc=[1884,1937,2471,2519,4456,4509,5043,5091,20858, 20857,22442, 22441,23484, 23483,20764, 20763 & 
& ,20860, 20859,22448, 22447,23492, 23491,20768, 20767], & 
& edgecnc=[7857,8649,9170,7810,7858,8652,9174,7812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2041),elname="xbrick",eltype="xbrick",typekey=2041) 

        call prepare(lib_xbrick(2042),key=2042, & 
& nodecnc=[131,2547,1940,2020,2703,5119,4512,4592,22930, 22929,23625, 23626,10766, 10765,10778, 10777 & 
& ,22934, 22933,23627, 23628,10774, 10773,10786, 10785], & 
& edgecnc=[8893,9241,2811,2817,8895,9242,2815,2821], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2042),elname="xbrick",eltype="xbrick",typekey=2042) 

        call prepare(lib_xbrick(2043),key=2043, & 
& nodecnc=[2547,1885,1938,1940,5119,4457,4510,4512,6746, 6745,23629, 23630,23631, 23632,23626, 23625,6754 & 
& , 6753,23633, 23634,23635, 23636,23628, 23627], & 
& edgecnc=[801,9243,9244,9241,805,9245,9246,9242], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2043),elname="xbrick",eltype="xbrick",typekey=2043) 

        call prepare(lib_xbrick(2044),key=2044, & 
& nodecnc=[2013,184,1886,1986,4585,2756,4458,4558,11742, 11741,6776, 6775,23422, 23421,23637, 23638,11748 & 
& , 11747,6784, 6783,23426, 23425,23639, 23640], & 
& edgecnc=[3299,816,9139,9247,3302,820,9141,9248], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2044),elname="xbrick",eltype="xbrick",typekey=2044) 

        call prepare(lib_xbrick(2045),key=2045, & 
& nodecnc=[1914,1887,2384,1812,4486,4459,4956,4384,12534, 12533,23641, 23642,22782, 22781,23374, 23373 & 
& ,12540, 12539,23643, 23644,22784, 22783,23376, 23375], & 
& edgecnc=[3695,9249,8819,9115,3698,9250,8820,9116], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2045),elname="xbrick",eltype="xbrick",typekey=2045) 

        call prepare(lib_xbrick(2046),key=2046, & 
& nodecnc=[2031,1954,2250,1889,4603,4526,4822,4461,7918, 7917,23645, 23646,7954, 7953,23647, 23648,7926 & 
& , 7925,23649, 23650,7960, 7959,23651, 23652], & 
& edgecnc=[1387,9251,1405,9252,1391,9253,1408,9254], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2046),elname="xbrick",eltype="xbrick",typekey=2046) 

        call prepare(lib_xbrick(2047),key=2047, & 
& nodecnc=[2031,1889,1821,263,4603,4461,4393,2835,23648, 23647,22856, 22855,23653, 23654,10604, 10603 & 
& ,23652, 23651,22862, 22861,23655, 23656,10612, 10611], & 
& edgecnc=[9252,8856,9255,2730,9254,8859,9256,2734], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2047),elname="xbrick",eltype="xbrick",typekey=2047) 

        call prepare(lib_xbrick(2048),key=2048, & 
& nodecnc=[2355,1890,1953,296,4927,4462,4525,2868,22848, 22847,12636, 12635,23657, 23658,23659, 23660 & 
& ,22852, 22851,12644, 12643,23661, 23662,23663, 23664], & 
& edgecnc=[8852,3746,9257,9258,8854,3750,9259,9260], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2048),elname="xbrick",eltype="xbrick",typekey=2048) 

        call prepare(lib_xbrick(2049),key=2049, & 
& nodecnc=[2047,295,1890,1975,4619,2867,4462,4547,7860, 7859,12638, 12637,22846, 22845,23665, 23666,7868 & 
& , 7867,12646, 12645,22850, 22849,23667, 23668], & 
& edgecnc=[1358,3747,8851,9261,1362,3751,8853,9262], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2049),elname="xbrick",eltype="xbrick",typekey=2049) 

        call prepare(lib_xbrick(2050),key=2050, & 
& nodecnc=[2052,287,1891,1976,4624,2859,4463,4548,5956, 5955,11970, 11969,5966, 5965,23669, 23670,5964 & 
& , 5963,11978, 11977,5974, 5973,23671, 23672], & 
& edgecnc=[406,3413,411,9263,410,3417,415,9264], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2050),elname="xbrick",eltype="xbrick",typekey=2050) 

        call prepare(lib_xbrick(2051),key=2051, & 
& nodecnc=[2033,322,1892,1951,4605,2894,4464,4523,12916, 12915,9120, 9119,12896, 12895,9016, 9015,12924 & 
& , 12923,9128, 9127,12904, 12903,9024, 9023], & 
& edgecnc=[3886,1988,3876,1936,3890,1992,3880,1940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2051),elname="xbrick",eltype="xbrick",typekey=2051) 

        call prepare(lib_xbrick(2052),key=2052, & 
& nodecnc=[2284,260,1893,1950,4856,2832,4465,4522,12468, 12467,6884, 6883,23673, 23674,12144, 12143,12476 & 
& , 12475,6892, 6891,23675, 23676,12152, 12151], & 
& edgecnc=[3662,870,9265,3500,3666,874,9266,3504], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2052),elname="xbrick",eltype="xbrick",typekey=2052) 

        call prepare(lib_xbrick(2053),key=2053, & 
& nodecnc=[2278,1894,1949,108,4850,4466,4521,2680,22818, 22817,23677, 23678,23679, 23680,23681, 23682 & 
& ,22824, 22823,23683, 23684,23685, 23686,23687, 23688], & 
& edgecnc=[8837,9267,9268,9269,8840,9270,9271,9272], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2053),elname="xbrick",eltype="xbrick",typekey=2053) 

        call prepare(lib_xbrick(2054),key=2054, & 
& nodecnc=[1907,155,1894,2299,4479,2727,4466,4871,23689, 23690,23691, 23692,23693, 23694,23014, 23013 & 
& ,23695, 23696,23697, 23698,23699, 23700,23020, 23019], & 
& edgecnc=[9273,9274,9275,8935,9276,9277,9278,8938], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2054),elname="xbrick",eltype="xbrick",typekey=2054) 

        call prepare(lib_xbrick(2055),key=2055, & 
& nodecnc=[2300,2301,2090,1897,4872,4873,4662,4469,23701, 23702,23703, 23704,23705, 23706,7688, 7687,23707 & 
& , 23708,23709, 23710,23711, 23712,7694, 7693], & 
& edgecnc=[9279,9280,9281,1272,9282,9283,9284,1275], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2055),elname="xbrick",eltype="xbrick",typekey=2055) 

        call prepare(lib_xbrick(2056),key=2056, & 
& nodecnc=[1948,2331,125,1898,4520,4903,2697,4470,23713, 23714,23715, 23716,23414, 23413,7490, 7489,23717 & 
& , 23718,23719, 23720,23416, 23415,7498, 7497], & 
& edgecnc=[9285,9286,9135,1173,9287,9288,9136,1177], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2056),elname="xbrick",eltype="xbrick",typekey=2056) 

        call prepare(lib_xbrick(2057),key=2057, & 
& nodecnc=[1947,1899,2360,2361,4519,4471,4932,4933,7028, 7027,22800, 22799,23721, 23722,10474, 10473,7036 & 
& , 7035,22806, 22805,23723, 23724,10482, 10481], & 
& edgecnc=[942,8828,9289,2665,946,8831,9290,2669], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2057),elname="xbrick",eltype="xbrick",typekey=2057) 

        call prepare(lib_xbrick(2058),key=2058, & 
& nodecnc=[1946,2328,192,1900,4518,4900,2764,4472,6140, 6139,12578, 12577,23418, 23417,9690, 9689,6148 & 
& , 6147,12584, 12583,23420, 23419,9692, 9691], & 
& edgecnc=[498,3717,9137,2273,502,3720,9138,2274], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2058),elname="xbrick",eltype="xbrick",typekey=2058) 

        call prepare(lib_xbrick(2059),key=2059, & 
& nodecnc=[1945,1901,2357,2358,4517,4473,4929,4930,12566, 12565,6072, 6071,23725, 23726,23727, 23728,12572 & 
& , 12571,6080, 6079,23729, 23730,23731, 23732], & 
& edgecnc=[3711,464,9291,9292,3714,468,9293,9294], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2059),elname="xbrick",eltype="xbrick",typekey=2059) 

        call prepare(lib_xbrick(2060),key=2060, & 
& nodecnc=[2305,1961,2002,1902,4877,4533,4574,4474,23733, 23734,23735, 23736,12202, 12201,23737, 23738 & 
& ,23739, 23740,23741, 23742,12208, 12207,23743, 23744], & 
& edgecnc=[9295,9296,3529,9297,9298,9299,3532,9300], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2060),elname="xbrick",eltype="xbrick",typekey=2060) 

        call prepare(lib_xbrick(2061),key=2061, & 
& nodecnc=[264,2305,1902,1836,2836,4877,4474,4408,23745, 23746,23738, 23737,22966, 22965,23747, 23748 & 
& ,23749, 23750,23744, 23743,22970, 22969,23751, 23752], & 
& edgecnc=[9301,9297,8911,9302,9303,9300,8913,9304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2061),elname="xbrick",eltype="xbrick",typekey=2061) 

        call prepare(lib_xbrick(2062),key=2062, & 
& nodecnc=[301,1903,1938,2025,2873,4475,4510,4597,23434, 23433,23753, 23754,23480, 23479,23755, 23756 & 
& ,23436, 23435,23757, 23758,23488, 23487,23759, 23760], & 
& edgecnc=[9145,9305,9168,9306,9146,9307,9172,9308], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2062),elname="xbrick",eltype="xbrick",typekey=2062) 

        call prepare(lib_xbrick(2063),key=2063, & 
& nodecnc=[2426,1971,1904,1944,4998,4543,4476,4516,23761, 23762,23188, 23187,23763, 23764,23765, 23766 & 
& ,23767, 23768,23192, 23191,23769, 23770,23771, 23772], & 
& edgecnc=[9309,9022,9310,9311,9312,9024,9313,9314], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2063),elname="xbrick",eltype="xbrick",typekey=2063) 

        call prepare(lib_xbrick(2064),key=2064, & 
& nodecnc=[2403,1904,1849,282,4975,4476,4421,2854,23773, 23774,23186, 23185,23310, 23309,9268, 9267,23775 & 
& , 23776,23190, 23189,23316, 23315,9276, 9275], & 
& edgecnc=[9315,9021,9083,2062,9316,9023,9086,2066], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2064),elname="xbrick",eltype="xbrick",typekey=2064) 

        call prepare(lib_xbrick(2065),key=2065, & 
& nodecnc=[2028,297,1905,2463,4600,2869,4477,5035,23777, 23778,22978, 22977,23779, 23780,23781, 23782 & 
& ,23783, 23784,22984, 22983,23785, 23786,23787, 23788], & 
& edgecnc=[9317,8917,9318,9319,9320,8920,9321,9322], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2065),elname="xbrick",eltype="xbrick",typekey=2065) 

        call prepare(lib_xbrick(2066),key=2066, & 
& nodecnc=[1958,2463,1905,1993,4530,5035,4477,4565,23789, 23790,23780, 23779,22986, 22985,23791, 23792 & 
& ,23793, 23794,23786, 23785,22988, 22987,23795, 23796], & 
& edgecnc=[9323,9318,8921,9324,9325,9321,8922,9326], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2066),elname="xbrick",eltype="xbrick",typekey=2066) 

        call prepare(lib_xbrick(2067),key=2067, & 
& nodecnc=[2083,2418,1906,1957,4655,4990,4478,4529,23797, 23798,23442, 23441,5786, 5785,23799, 23800,23801 & 
& , 23802,23448, 23447,5794, 5793,23803, 23804], & 
& edgecnc=[9327,9149,321,9328,9329,9152,325,9330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2067),elname="xbrick",eltype="xbrick",typekey=2067) 

        call prepare(lib_xbrick(2068),key=2068, & 
& nodecnc=[1973,265,1935,1943,4545,2837,4507,4515,23438, 23437,23338, 23337,23805, 23806,23807, 23808 & 
& ,23440, 23439,23346, 23345,23809, 23810,23811, 23812], & 
& edgecnc=[9147,9097,9331,9332,9148,9101,9333,9334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2068),elname="xbrick",eltype="xbrick",typekey=2068) 

        call prepare(lib_xbrick(2069),key=2069, & 
& nodecnc=[2077,1942,298,2000,4649,4514,2870,4572,23813, 23814,8008, 8007,23198, 23197,23815, 23816,23817 & 
& , 23818,8016, 8015,23204, 23203,23819, 23820], & 
& edgecnc=[9335,1432,9027,9336,9337,1436,9030,9338], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2069),elname="xbrick",eltype="xbrick",typekey=2069) 

        call prepare(lib_xbrick(2070),key=2070, & 
& nodecnc=[2458,1941,284,1999,5030,4513,2856,4571,23821, 23822,10252, 10251,23210, 23209,23823, 23824 & 
& ,23825, 23826,10260, 10259,23216, 23215,23827, 23828], & 
& edgecnc=[9339,2554,9033,9340,9341,2558,9036,9342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2070),elname="xbrick",eltype="xbrick",typekey=2070) 

        call prepare(lib_xbrick(2071),key=2071, & 
& nodecnc=[1923,1966,2011,323,4495,4538,4583,2895,12912, 12911,23829, 23830,23831, 23832,23002, 23001 & 
& ,12920, 12919,23833, 23834,23835, 23836,23008, 23007], & 
& edgecnc=[3884,9343,9344,8929,3888,9345,9346,8932], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2071),elname="xbrick",eltype="xbrick",typekey=2071) 

        call prepare(lib_xbrick(2072),key=2072, & 
& nodecnc=[1922,1967,2010,259,4494,4539,4582,2831,12464, 12463,23837, 23838,23839, 23840,23034, 23033 & 
& ,12472, 12471,23841, 23842,23843, 23844,23040, 23039], & 
& edgecnc=[3660,9347,9348,8945,3664,9349,9350,8948], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2072),elname="xbrick",eltype="xbrick",typekey=2072) 

        call prepare(lib_xbrick(2073),key=2073, & 
& nodecnc=[1921,1964,2009,293,4493,4536,4581,2865,23845, 23846,23847, 23848,23849, 23850,23062, 23061 & 
& ,23851, 23852,23853, 23854,23855, 23856,23068, 23067], & 
& edgecnc=[9351,9352,9353,8959,9354,9355,9356,8962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2073),elname="xbrick",eltype="xbrick",typekey=2073) 

        call prepare(lib_xbrick(2074),key=2074, & 
& nodecnc=[1920,1963,2008,290,4492,4535,4580,2862,12574, 12573,12554, 12553,23857, 23858,23114, 23113 & 
& ,12580, 12579,12560, 12559,23859, 23860,23120, 23119], & 
& edgecnc=[3715,3705,9357,8985,3718,3708,9358,8988], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2074),elname="xbrick",eltype="xbrick",typekey=2074) 

        call prepare(lib_xbrick(2075),key=2075, & 
& nodecnc=[281,2060,1983,1929,2853,4632,4555,4501,23861, 23862,23863, 23864,23504, 23503,5688, 5687,23865 & 
& , 23866,23867, 23868,23510, 23509,5696, 5695], & 
& edgecnc=[9359,9360,9180,272,9361,9362,9183,276], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2075),elname="xbrick",eltype="xbrick",typekey=2075) 

        call prepare(lib_xbrick(2076),key=2076, & 
& nodecnc=[254,2277,1911,1672,2826,4849,4483,4244,6086, 6085,23564, 23563,22006, 22005,22602, 22601,6094 & 
& , 6093,23568, 23567,22012, 22011,22608, 22607], & 
& edgecnc=[471,9210,8431,8729,475,9212,8434,8732], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2076),elname="xbrick",eltype="xbrick",typekey=2076) 

        call prepare(lib_xbrick(2077),key=2077, & 
& nodecnc=[328,2276,1912,1673,2900,4848,4484,4245,12942, 12941,23554, 23553,7830, 7829,22612, 22611,12946 & 
& , 12945,23558, 23557,7836, 7835,22616, 22615], & 
& edgecnc=[3899,9205,1343,8734,3901,9207,1346,8736], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2077),elname="xbrick",eltype="xbrick",typekey=2077) 

        call prepare(lib_xbrick(2078),key=2078, & 
& nodecnc=[1969,1859,1874,1913,4541,4431,4446,4485,23496, 23495,10140, 10139,5814, 5813,23869, 23870,23500 & 
& , 23499,10146, 10145,5822, 5821,23871, 23872], & 
& edgecnc=[9176,2498,335,9363,9178,2501,339,9364], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2078),elname="xbrick",eltype="xbrick",typekey=2078) 

        call prepare(lib_xbrick(2079),key=2079, & 
& nodecnc=[2383,1935,1970,1916,4955,4507,4542,4488,23873, 23874,23344, 23343,23462, 23461,23472, 23471 & 
& ,23875, 23876,23352, 23351,23466, 23465,23476, 23475], & 
& edgecnc=[9365,9100,9159,9164,9366,9104,9161,9166], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2079),elname="xbrick",eltype="xbrick",typekey=2079) 

        call prepare(lib_xbrick(2080),key=2080, & 
& nodecnc=[1974,2570,165,1425,4546,5142,2737,3997,23877, 23878,23879, 23880,7208, 7207,11714, 11713,23881 & 
& , 23882,23883, 23884,7216, 7215,11722, 11721], & 
& edgecnc=[9367,9368,1032,3285,9369,9370,1036,3289], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2080),elname="xbrick",eltype="xbrick",typekey=2080) 

        call prepare(lib_xbrick(2081),key=2081, & 
& nodecnc=[2436,2026,2020,1960,5008,4598,4592,4532,10700, 10699,10780, 10779,10764, 10763,23596, 23595 & 
& ,10708, 10707,10788, 10787,10772, 10771,23600, 23599], & 
& edgecnc=[2778,2818,2810,9226,2782,2822,2814,9228], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2081),elname="xbrick",eltype="xbrick",typekey=2081) 

        call prepare(lib_xbrick(2082),key=2082, & 
& nodecnc=[2098,2047,1975,329,4670,4619,4547,2901,23885, 23886,23666, 23665,10518, 10517,22108, 22107 & 
& ,23887, 23888,23668, 23667,10524, 10523,22112, 22111], & 
& edgecnc=[9371,9261,2687,8482,9372,9262,2690,8484], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2082),elname="xbrick",eltype="xbrick",typekey=2082) 

        call prepare(lib_xbrick(2083),key=2083, & 
& nodecnc=[2098,1924,2004,2047,4670,4496,4576,4619,23556, 23555,7842, 7841,7854, 7853,23886, 23885,23560 & 
& , 23559,7848, 7847,7862, 7861,23888, 23887], & 
& edgecnc=[9206,1349,1355,9371,9208,1352,1359,9372], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2083),elname="xbrick",eltype="xbrick",typekey=2083) 

        call prepare(lib_xbrick(2084),key=2084, & 
& nodecnc=[2246,1980,189,2005,4818,4552,2761,4577,23889, 23890,7458, 7457,23891, 23892,23893, 23894,23895 & 
& , 23896,7466, 7465,23897, 23898,23899, 23900], & 
& edgecnc=[9373,1157,9374,9375,9376,1161,9377,9378], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2084),elname="xbrick",eltype="xbrick",typekey=2084) 

        call prepare(lib_xbrick(2085),key=2085, & 
& nodecnc=[1562,1561,2108,1932,4134,4133,4680,4504,22064, 22063,7770, 7769,22202, 22201,22214, 22213,22068 & 
& , 22067,7778, 7777,22210, 22209,22218, 22217], & 
& edgecnc=[8460,1313,8529,8535,8462,1317,8533,8537], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2085),elname="xbrick",eltype="xbrick",typekey=2085) 

        call prepare(lib_xbrick(2086),key=2086, & 
& nodecnc=[1926,2050,2102,2007,4498,4622,4674,4579,23901, 23902,23903, 23904,5938, 5937,9596, 9595,23905 & 
& , 23906,23907, 23908,5946, 5945,9604, 9603], & 
& edgecnc=[9379,9380,397,2226,9381,9382,401,2230], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2086),elname="xbrick",eltype="xbrick",typekey=2086) 

        call prepare(lib_xbrick(2087),key=2087, & 
& nodecnc=[1582,1581,2107,1933,4154,4153,4679,4505,22132, 22131,9638, 9637,23909, 23910,22270, 22269,22136 & 
& , 22135,9646, 9645,23911, 23912,22272, 22271], & 
& edgecnc=[8494,2247,9383,8563,8496,2251,9384,8564], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2087),elname="xbrick",eltype="xbrick",typekey=2087) 

        call prepare(lib_xbrick(2088),key=2088, & 
& nodecnc=[2101,2052,1976,253,4673,4624,4548,2825,23913, 23914,23670, 23669,22842, 22841,6224, 6223,23915 & 
& , 23916,23672, 23671,22844, 22843,6232, 6231], & 
& edgecnc=[9385,9263,8849,540,9386,9264,8850,544], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2088),elname="xbrick",eltype="xbrick",typekey=2088) 

        call prepare(lib_xbrick(2089),key=2089, & 
& nodecnc=[2101,1927,2006,2052,4673,4499,4578,4624,23562, 23561,6102, 6101,5950, 5949,23914, 23913,23566 & 
& , 23565,6108, 6107,5958, 5957,23916, 23915], & 
& edgecnc=[9209,479,403,9385,9211,482,407,9386], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2089),elname="xbrick",eltype="xbrick",typekey=2089) 

        call prepare(lib_xbrick(2090),key=2090, & 
& nodecnc=[1930,2174,2566,1368,4502,4746,5138,3940,23917, 23918,23919, 23920,21280, 21279,10336, 10335 & 
& ,23921, 23922,23923, 23924,21284, 21283,10344, 10343], & 
& edgecnc=[9387,9388,8068,2596,9389,9390,8070,2600], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2090),elname="xbrick",eltype="xbrick",typekey=2090) 

        call prepare(lib_xbrick(2091),key=2091, & 
& nodecnc=[2563,2164,1981,2057,5135,4736,4553,4629,23925, 23926,23530, 23529,23927, 23928,23929, 23930 & 
& ,23931, 23932,23536, 23535,23933, 23934,23935, 23936], & 
& edgecnc=[9391,9193,9392,9393,9394,9196,9395,9396], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2091),elname="xbrick",eltype="xbrick",typekey=2091) 

        call prepare(lib_xbrick(2092),key=2092, & 
& nodecnc=[2005,1925,2245,2246,4577,4497,4817,4818,6914, 6913,7476, 7475,22198, 22197,23894, 23893,6922 & 
& , 6921,7484, 7483,22206, 22205,23900, 23899], & 
& edgecnc=[885,1166,8527,9375,889,1170,8531,9378], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2092),elname="xbrick",eltype="xbrick",typekey=2092) 

        call prepare(lib_xbrick(2093),key=2093, & 
& nodecnc=[2244,2243,1933,2107,4816,4815,4505,4679,23937, 23938,22260, 22259,23910, 23909,23939, 23940 & 
& ,23941, 23942,22266, 22265,23912, 23911,23943, 23944], & 
& edgecnc=[9397,8558,9383,9398,9399,8561,9384,9400], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2093),elname="xbrick",eltype="xbrick",typekey=2093) 

        call prepare(lib_xbrick(2094),key=2094, & 
& nodecnc=[1559,1558,245,1774,4131,4130,2817,4346,22180, 22179,22188, 22187,23158, 23157,22142, 22141 & 
& ,22184, 22183,22192, 22191,23162, 23161,22146, 22145], & 
& edgecnc=[8518,8522,9007,8499,8520,8524,9009,8501], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2094),elname="xbrick",eltype="xbrick",typekey=2094) 

        call prepare(lib_xbrick(2095),key=2095, & 
& nodecnc=[2555,1936,99,1359,5127,4508,2671,3931,21394, 21393,21070, 21069,20910, 20909,20170, 20169,21398 & 
& , 21397,21076, 21075,20914, 20913,20176, 20175], & 
& edgecnc=[8125,7963,7883,7513,8127,7966,7885,7516], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2095),elname="xbrick",eltype="xbrick",typekey=2095) 

        call prepare(lib_xbrick(2096),key=2096, & 
& nodecnc=[2519,1938,1885,1858,5091,4510,4457,4430,23482, 23481,23630, 23629,6744, 6743,20762, 20761,23490 & 
& , 23489,23634, 23633,6752, 6751,20766, 20765], & 
& edgecnc=[9169,9243,800,7809,9173,9245,804,7811], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2096),elname="xbrick",eltype="xbrick",typekey=2096) 

        call prepare(lib_xbrick(2097),key=2097, & 
& nodecnc=[1940,1938,1903,300,4512,4510,4475,2872,23632, 23631,23754, 23753,23166, 23165,23945, 23946 & 
& ,23636, 23635,23758, 23757,23172, 23171,23947, 23948], & 
& edgecnc=[9244,9305,9011,9401,9246,9307,9014,9402], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2097),elname="xbrick",eltype="xbrick",typekey=2097) 

        call prepare(lib_xbrick(2098),key=2098, & 
& nodecnc=[2012,153,1939,1969,4584,2725,4511,4541,5830, 5829,23360, 23359,23494, 23493,23949, 23950,5838 & 
& , 5837,23366, 23365,23498, 23497,23951, 23952], & 
& edgecnc=[343,9108,9175,9403,347,9111,9177,9404], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2098),elname="xbrick",eltype="xbrick",typekey=2098) 

        call prepare(lib_xbrick(2099),key=2099, & 
& nodecnc=[2513,2055,2076,197,5085,4627,4648,2769,23953, 23954,10312, 10311,23955, 23956,23957, 23958 & 
& ,23959, 23960,10318, 10317,23961, 23962,23963, 23964], & 
& edgecnc=[9405,2584,9406,9407,9408,2587,9409,9410], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2099),elname="xbrick",eltype="xbrick",typekey=2099) 

        call prepare(lib_xbrick(2100),key=2100, & 
& nodecnc=[1942,2077,2490,2074,4514,4649,5062,4646,23814, 23813,10664, 10663,23965, 23966,23967, 23968 & 
& ,23818, 23817,10672, 10671,23969, 23970,23971, 23972], & 
& edgecnc=[9335,2760,9411,9412,9337,2764,9413,9414], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2100),elname="xbrick",eltype="xbrick",typekey=2100) 

        call prepare(lib_xbrick(2101),key=2101, & 
& nodecnc=[2305,264,2044,2030,4877,2836,4616,4602,23746, 23745,23973, 23974,10620, 10619,23975, 23976 & 
& ,23750, 23749,23977, 23978,10628, 10627,23979, 23980], & 
& edgecnc=[9301,9415,2738,9416,9303,9417,2742,9418], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2101),elname="xbrick",eltype="xbrick",typekey=2101) 

        call prepare(lib_xbrick(2102),key=2102, & 
& nodecnc=[2426,1944,2072,2061,4998,4516,4644,4633,23766, 23765,23981, 23982,8926, 8925,8946, 8945,23772 & 
& , 23771,23983, 23984,8934, 8933,8952, 8951], & 
& edgecnc=[9311,9419,1891,1901,9314,9420,1895,1904], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2102),elname="xbrick",eltype="xbrick",typekey=2102) 

        call prepare(lib_xbrick(2103),key=2103, & 
& nodecnc=[1904,2403,2021,1944,4476,4975,4593,4516,23774, 23773,23985, 23986,23987, 23988,23764, 23763 & 
& ,23776, 23775,23989, 23990,23991, 23992,23770, 23769], & 
& edgecnc=[9315,9421,9422,9310,9316,9423,9424,9313], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2103),elname="xbrick",eltype="xbrick",typekey=2103) 

        call prepare(lib_xbrick(2104),key=2104, & 
& nodecnc=[2099,1945,2358,2092,4671,4517,4930,4664,23993, 23994,23728, 23727,23995, 23996,23997, 23998 & 
& ,23999, 24000,23732, 23731,24001, 24002,24003, 24004], & 
& edgecnc=[9425,9292,9426,9427,9428,9294,9429,9430], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2104),elname="xbrick",eltype="xbrick",typekey=2104) 

        call prepare(lib_xbrick(2105),key=2105, & 
& nodecnc=[1964,2015,2037,2009,4536,4587,4609,4581,10492, 10491,24005, 24006,24007, 24008,23848, 23847 & 
& ,10498, 10497,24009, 24010,24011, 24012,23854, 23853], & 
& edgecnc=[2674,9431,9432,9352,2677,9433,9434,9355], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2105),elname="xbrick",eltype="xbrick",typekey=2105) 

        call prepare(lib_xbrick(2106),key=2106, & 
& nodecnc=[2089,2036,2331,1948,4661,4608,4903,4520,6928, 6927,10490, 10489,23714, 23713,10462, 10461,6936 & 
& , 6935,10496, 10495,23718, 23717,10468, 10467], & 
& edgecnc=[892,2673,9285,2659,896,2676,9287,2662], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2106),elname="xbrick",eltype="xbrick",typekey=2106) 

        call prepare(lib_xbrick(2107),key=2107, & 
& nodecnc=[188,2122,2103,2126,2760,4694,4675,4698,11784, 11783,24013, 24014,24015, 24016,24017, 24018 & 
& ,11792, 11791,24019, 24020,24021, 24022,24023, 24024], & 
& edgecnc=[3320,9435,9436,9437,3324,9438,9439,9440], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2107),elname="xbrick",eltype="xbrick",typekey=2107) 

        call prepare(lib_xbrick(2108),key=2108, & 
& nodecnc=[2034,1949,1894,155,4606,4521,4466,2727,24025, 24026,23678, 23677,23692, 23691,5848, 5847,24027 & 
& , 24028,23684, 23683,23698, 23697,5856, 5855], & 
& edgecnc=[9441,9267,9274,352,9442,9270,9277,356], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2108),elname="xbrick",eltype="xbrick",typekey=2108) 

        call prepare(lib_xbrick(2109),key=2109, & 
& nodecnc=[1949,2128,2387,108,4521,4700,4959,2680,24029, 24030,24031, 24032,24033, 24034,23680, 23679 & 
& ,24035, 24036,24037, 24038,24039, 24040,23686, 23685], & 
& edgecnc=[9443,9444,9445,9268,9446,9447,9448,9271], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2109),elname="xbrick",eltype="xbrick",typekey=2109) 

        call prepare(lib_xbrick(2110),key=2110, & 
& nodecnc=[2035,1950,2133,2087,4607,4522,4705,4659,12146, 12145,24041, 24042,24043, 24044,6898, 6897,12154 & 
& , 12153,24045, 24046,24047, 24048,6906, 6905], & 
& edgecnc=[3501,9449,9450,877,3505,9451,9452,881], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2110),elname="xbrick",eltype="xbrick",typekey=2110) 

        call prepare(lib_xbrick(2111),key=2111, & 
& nodecnc=[2133,1950,1893,261,4705,4522,4465,2833,24042, 24041,23674, 23673,22830, 22829,12158, 12157 & 
& ,24046, 24045,23676, 23675,22834, 22833,12166, 12165], & 
& edgecnc=[9449,9265,8843,3507,9451,9266,8845,3511], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2111),elname="xbrick",eltype="xbrick",typekey=2111) 

        call prepare(lib_xbrick(2112),key=2112, & 
& nodecnc=[2239,2001,2046,321,4811,4573,4618,2893,23456, 23455,24049, 24050,12900, 12899,24051, 24052 & 
& ,23460, 23459,24053, 24054,12908, 12907,24055, 24056], & 
& edgecnc=[9156,9453,3878,9454,9158,9455,3882,9456], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2112),elname="xbrick",eltype="xbrick",typekey=2112) 

        call prepare(lib_xbrick(2113),key=2113, & 
& nodecnc=[1957,286,1952,2032,4529,2858,4524,4604,5784, 5783,23406, 23405,11988, 11987,24057, 24058,5792 & 
& , 5791,23410, 23409,11996, 11995,24059, 24060], & 
& edgecnc=[320,9131,3422,9457,324,9133,3426,9458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2113),elname="xbrick",eltype="xbrick",typekey=2113) 

        call prepare(lib_xbrick(2114),key=2114, & 
& nodecnc=[195,2084,1952,2127,2767,4656,4524,4699,24061, 24062,11982, 11981,11966, 11965,24063, 24064 & 
& ,24065, 24066,11990, 11989,11974, 11973,24067, 24068], & 
& edgecnc=[9459,3419,3411,9460,9461,3423,3415,9462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2114),elname="xbrick",eltype="xbrick",typekey=2114) 

        call prepare(lib_xbrick(2115),key=2115, & 
& nodecnc=[2080,296,1953,2029,4652,2868,4525,4601,12654, 12653,23658, 23657,24069, 24070,10546, 10545 & 
& ,12662, 12661,23662, 23661,24071, 24072,10554, 10553], & 
& edgecnc=[3755,9257,9463,2701,3759,9259,9464,2705], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2115),elname="xbrick",eltype="xbrick",typekey=2115) 

        call prepare(lib_xbrick(2116),key=2116, & 
& nodecnc=[2081,2029,1953,2125,4653,4601,4525,4697,10548, 10547,24070, 24069,12634, 12633,24073, 24074 & 
& ,10556, 10555,24072, 24071,12642, 12641,24075, 24076], & 
& edgecnc=[2702,9463,3745,9465,2706,9464,3749,9466], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2116),elname="xbrick",eltype="xbrick",typekey=2116) 

        call prepare(lib_xbrick(2117),key=2117, & 
& nodecnc=[1956,2158,2106,2110,4528,4730,4678,4682,24077, 24078,10750, 10749,24079, 24080,10704, 10703 & 
& ,24081, 24082,10756, 10755,24083, 24084,10712, 10711], & 
& edgecnc=[9467,2803,9468,2780,9469,2806,9470,2784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2117),elname="xbrick",eltype="xbrick",typekey=2117) 

        call prepare(lib_xbrick(2118),key=2118, & 
& nodecnc=[2158,1956,1990,2376,4730,4528,4562,4948,24078, 24077,23602, 23601,24085, 24086,24087, 24088 & 
& ,24082, 24081,23604, 23603,24089, 24090,24091, 24092], & 
& edgecnc=[9467,9229,9471,9472,9469,9230,9473,9474], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2118),elname="xbrick",eltype="xbrick",typekey=2118) 

        call prepare(lib_xbrick(2119),key=2119, & 
& nodecnc=[2504,2062,2083,196,5076,4634,4655,2768,8978, 8977,24093, 24094,24095, 24096,24097, 24098,8986 & 
& , 8985,24099, 24100,24101, 24102,24103, 24104], & 
& edgecnc=[1917,9475,9476,9477,1921,9478,9479,9480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2119),elname="xbrick",eltype="xbrick",typekey=2119) 

        call prepare(lib_xbrick(2120),key=2120, & 
& nodecnc=[2078,2028,2463,1958,4650,4600,5035,4530,10660, 10659,23782, 23781,23790, 23789,12650, 12649 & 
& ,10668, 10667,23788, 23787,23794, 23793,12658, 12657], & 
& edgecnc=[2758,9319,9323,3753,2762,9322,9325,3757], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2120),elname="xbrick",eltype="xbrick",typekey=2120) 

        call prepare(lib_xbrick(2121),key=2121, & 
& nodecnc=[128,2143,2078,2080,2700,4715,4650,4652,24105, 24106,11806, 11805,12656, 12655,10552, 10551 & 
& ,24107, 24108,11812, 11811,12664, 12663,10560, 10559], & 
& edgecnc=[9481,3331,3756,2704,9482,3334,3760,2708], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2121),elname="xbrick",eltype="xbrick",typekey=2121) 

        call prepare(lib_xbrick(2122),key=2122, & 
& nodecnc=[2493,2027,2119,2075,5065,4599,4691,4647,24109, 24110,24111, 24112,24113, 24114,9284, 9283,24115 & 
& , 24116,24117, 24118,24119, 24120,9290, 9289], & 
& edgecnc=[9483,9484,9485,2070,9486,9487,9488,2073], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2122),elname="xbrick",eltype="xbrick",typekey=2122) 

        call prepare(lib_xbrick(2123),key=2123, & 
& nodecnc=[283,2467,1959,1879,2855,5039,4531,4451,23606, 23605,24121, 24122,24123, 24124,23322, 23321 & 
& ,23612, 23611,24125, 24126,24127, 24128,23324, 23323], & 
& edgecnc=[9231,9489,9490,9089,9234,9491,9492,9090], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2123),elname="xbrick",eltype="xbrick",typekey=2123) 

        call prepare(lib_xbrick(2124),key=2124, & 
& nodecnc=[2305,2030,2079,1961,4877,4602,4651,4533,23976, 23975,24129, 24130,24131, 24132,23734, 23733 & 
& ,23980, 23979,24133, 24134,24135, 24136,23740, 23739], & 
& edgecnc=[9416,9493,9494,9295,9418,9495,9496,9298], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2124),elname="xbrick",eltype="xbrick",typekey=2124) 

        call prepare(lib_xbrick(2125),key=2125, & 
& nodecnc=[114,2516,2526,1998,2686,5088,5098,4570,23382, 23381,24137, 24138,23396, 23395,24139, 24140 & 
& ,23386, 23385,24141, 24142,23402, 23401,24143, 24144], & 
& edgecnc=[9119,9497,9126,9498,9121,9499,9129,9500], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2125),elname="xbrick",eltype="xbrick",typekey=2125) 

        call prepare(lib_xbrick(2126),key=2126, & 
& nodecnc=[289,1910,290,2008,2861,4482,2862,4580,24145, 24146,23102, 23101,23858, 23857,24147, 24148,24149 & 
& , 24150,23106, 23105,23860, 23859,24151, 24152], & 
& edgecnc=[9501,8979,9357,9502,9503,8981,9358,9504], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2126),elname="xbrick",eltype="xbrick",typekey=2126) 

        call prepare(lib_xbrick(2127),key=2127, & 
& nodecnc=[2411,289,2008,2039,4983,2861,4580,4611,24153, 24154,24148, 24147,12552, 12551,24155, 24156 & 
& ,24157, 24158,24152, 24151,12558, 12557,24159, 24160], & 
& edgecnc=[9505,9502,3704,9506,9507,9504,3707,9508], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2127),elname="xbrick",eltype="xbrick",typekey=2127) 

        call prepare(lib_xbrick(2128),key=2128, & 
& nodecnc=[2331,1964,1921,125,4903,4536,4493,2697,10494, 10493,23846, 23845,23070, 23069,23716, 23715 & 
& ,10500, 10499,23852, 23851,23074, 23073,23720, 23719], & 
& edgecnc=[2675,9351,8963,9286,2678,9354,8965,9288], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2128),elname="xbrick",eltype="xbrick",typekey=2128) 

        call prepare(lib_xbrick(2129),key=2129, & 
& nodecnc=[2037,1909,293,2009,4609,4481,2865,4581,24161, 24162,23086, 23085,23850, 23849,24008, 24007 & 
& ,24163, 24164,23090, 23089,23856, 23855,24012, 24011], & 
& edgecnc=[9509,8971,9353,9432,9510,8973,9356,9434], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2129),elname="xbrick",eltype="xbrick",typekey=2129) 

        call prepare(lib_xbrick(2130),key=2130, & 
& nodecnc=[2019,1965,2130,218,4591,4537,4702,2790,6790, 6789,10594, 10593,24165, 24166,24167, 24168,6796 & 
& , 6795,10600, 10599,24169, 24170,24171, 24172], & 
& edgecnc=[823,2725,9511,9512,826,2728,9513,9514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2130),elname="xbrick",eltype="xbrick",typekey=2130) 

        call prepare(lib_xbrick(2131),key=2131, & 
& nodecnc=[155,1907,323,2011,2727,4479,2895,4583,23690, 23689,23022, 23021,23832, 23831,5850, 5849,23696 & 
& , 23695,23026, 23025,23836, 23835,5858, 5857], & 
& edgecnc=[9273,8939,9344,353,9276,8941,9346,357], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2131),elname="xbrick",eltype="xbrick",typekey=2131) 

        call prepare(lib_xbrick(2132),key=2132, & 
& nodecnc=[2011,1966,2344,2017,4583,4538,4916,4589,23830, 23829,24173, 24174,24175, 24176,5852, 5851,23834 & 
& , 23833,24177, 24178,24179, 24180,5860, 5859], & 
& edgecnc=[9343,9515,9516,354,9345,9517,9518,358], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2132),elname="xbrick",eltype="xbrick",typekey=2132) 

        call prepare(lib_xbrick(2133),key=2133, & 
& nodecnc=[223,1908,259,2010,2795,4480,2831,4582,24181, 24182,23050, 23049,23840, 23839,7444, 7443,24183 & 
& , 24184,23054, 23053,23844, 23843,7452, 7451], & 
& edgecnc=[9519,8953,9348,1150,9520,8955,9350,1154], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2133),elname="xbrick",eltype="xbrick",typekey=2133) 

        call prepare(lib_xbrick(2134),key=2134, & 
& nodecnc=[2470,1968,1405,101,5042,4540,3977,2673,24185, 24186,21516, 21515,21318, 21317,22406, 22405 & 
& ,24187, 24188,21520, 21519,21322, 21321,22414, 22413], & 
& edgecnc=[9521,8186,8087,8631,9522,8188,8089,8635], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2134),elname="xbrick",eltype="xbrick",typekey=2134) 

        call prepare(lib_xbrick(2135),key=2135, & 
& nodecnc=[2012,1969,1913,106,4584,4541,4485,2678,23950, 23949,23870, 23869,24189, 24190,24191, 24192 & 
& ,23952, 23951,23872, 23871,24193, 24194,24195, 24196], & 
& edgecnc=[9403,9363,9523,9524,9404,9364,9525,9526], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2135),elname="xbrick",eltype="xbrick",typekey=2135) 

        call prepare(lib_xbrick(2136),key=2136, & 
& nodecnc=[2426,2060,281,1971,4998,4632,2853,4543,8944, 8943,23862, 23861,23180, 23179,23762, 23761,8950 & 
& , 8949,23866, 23865,23184, 23183,23768, 23767], & 
& edgecnc=[1900,9359,9018,9309,1903,9361,9020,9312], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2136),elname="xbrick",eltype="xbrick",typekey=2136) 

        call prepare(lib_xbrick(2137),key=2137, & 
& nodecnc=[262,2058,228,1972,2834,4630,2800,4544,24197, 24198,7114, 7113,7938, 7937,24199, 24200,24201 & 
& , 24202,7120, 7119,7946, 7945,24203, 24204], & 
& edgecnc=[9527,985,1397,9528,9529,988,1401,9530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2137),elname="xbrick",eltype="xbrick",typekey=2137) 

        call prepare(lib_xbrick(2138),key=2138, & 
& nodecnc=[1954,262,1972,2250,4526,2834,4544,4822,24205, 24206,24200, 24199,7950, 7949,23646, 23645,24207 & 
& , 24208,24204, 24203,7956, 7955,23650, 23649], & 
& edgecnc=[9531,9528,1403,9251,9532,9530,1406,9253], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2138),elname="xbrick",eltype="xbrick",typekey=2138) 

        call prepare(lib_xbrick(2139),key=2139, & 
& nodecnc=[1943,264,1836,1973,4515,2836,4408,4545,24209, 24210,23748, 23747,22758, 22757,23808, 23807 & 
& ,24211, 24212,23752, 23751,22764, 22763,23812, 23811], & 
& edgecnc=[9533,9302,8807,9332,9534,9304,8810,9334], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2139),elname="xbrick",eltype="xbrick",typekey=2139) 

        call prepare(lib_xbrick(2140),key=2140, & 
& nodecnc=[1977,2063,2239,321,4549,4635,4811,2893,24213, 24214,24215, 24216,24052, 24051,22838, 22837 & 
& ,24217, 24218,24219, 24220,24056, 24055,22840, 22839], & 
& edgecnc=[9535,9536,9454,8847,9537,9538,9456,8848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2140),elname="xbrick",eltype="xbrick",typekey=2140) 

        call prepare(lib_xbrick(2141),key=2141, & 
& nodecnc=[261,1978,2059,2115,2833,4550,4631,4687,22832, 22831,24221, 24222,7128, 7127,12160, 12159,22836 & 
& , 22835,24223, 24224,7134, 7133,12168, 12167], & 
& edgecnc=[8844,9539,992,3508,8846,9540,995,3512], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2141),elname="xbrick",eltype="xbrick",typekey=2141) 

        call prepare(lib_xbrick(2142),key=2142, & 
& nodecnc=[2107,92,1979,2244,4679,2664,4551,4816,9636, 9635,10184, 10183,24225, 24226,23940, 23939,9644 & 
& , 9643,10192, 10191,24227, 24228,23944, 23943], & 
& edgecnc=[2246,2520,9541,9398,2250,2524,9542,9400], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2142),elname="xbrick",eltype="xbrick",typekey=2142) 

        call prepare(lib_xbrick(2143),key=2143, & 
& nodecnc=[2244,1979,2278,2050,4816,4551,4850,4622,24226, 24225,22814, 22813,24229, 24230,24231, 24232 & 
& ,24228, 24227,22820, 22819,24233, 24234,24235, 24236], & 
& edgecnc=[9541,8835,9543,9544,9542,8838,9545,9546], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2143),elname="xbrick",eltype="xbrick",typekey=2143) 

        call prepare(lib_xbrick(2144),key=2144, & 
& nodecnc=[2108,93,1980,2246,4680,2665,4552,4818,7768, 7767,22630, 22629,23890, 23889,22204, 22203,7776 & 
& , 7775,22634, 22633,23896, 23895,22212, 22211], & 
& edgecnc=[1312,8743,9373,8530,1316,8745,9376,8534], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2144),elname="xbrick",eltype="xbrick",typekey=2144) 

        call prepare(lib_xbrick(2145),key=2145, & 
& nodecnc=[2057,1981,2522,201,4629,4553,5094,2773,23928, 23927,5672, 5671,24237, 24238,24239, 24240,23934 & 
& , 23933,5680, 5679,24241, 24242,24243, 24244], & 
& edgecnc=[9392,264,9547,9548,9395,268,9549,9550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2145),elname="xbrick",eltype="xbrick",typekey=2145) 

        call prepare(lib_xbrick(2146),key=2146, & 
& nodecnc=[197,2523,1883,2513,2769,5095,4455,5085,24245, 24246,12178, 12177,24247, 24248,23958, 23957 & 
& ,24249, 24250,12184, 12183,24251, 24252,23964, 23963], & 
& edgecnc=[9551,3517,9552,9407,9553,3520,9554,9410], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2146),elname="xbrick",eltype="xbrick",typekey=2146) 

        call prepare(lib_xbrick(2147),key=2147, & 
& nodecnc=[1883,1982,2055,2513,4455,4554,4627,5085,23522, 23521,24253, 24254,23954, 23953,24248, 24247 & 
& ,23524, 23523,24255, 24256,23960, 23959,24252, 24251], & 
& edgecnc=[9189,9555,9405,9552,9190,9556,9408,9554], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2147),elname="xbrick",eltype="xbrick",typekey=2147) 

        call prepare(lib_xbrick(2148),key=2148, & 
& nodecnc=[201,2522,2462,1983,2773,5094,5034,4555,24238, 24237,12000, 11999,23506, 23505,24257, 24258 & 
& ,24242, 24241,12006, 12005,23512, 23511,24259, 24260], & 
& edgecnc=[9547,3428,9181,9557,9549,3431,9184,9558], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2148),elname="xbrick",eltype="xbrick",typekey=2148) 

        call prepare(lib_xbrick(2149),key=2149, & 
& nodecnc=[2060,2485,201,1983,4632,5057,2773,4555,8942, 8941,24261, 24262,24258, 24257,23864, 23863,8948 & 
& , 8947,24263, 24264,24260, 24259,23868, 23867], & 
& edgecnc=[1899,9559,9557,9360,1902,9560,9558,9362], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2149),elname="xbrick",eltype="xbrick",typekey=2149) 

        call prepare(lib_xbrick(2150),key=2150, & 
& nodecnc=[2063,1984,1928,2239,4635,4556,4500,4811,9394, 9393,9190, 9189,23454, 23453,24216, 24215,9400 & 
& , 9399,9196, 9195,23458, 23457,24220, 24219], & 
& edgecnc=[2125,2023,9155,9536,2128,2026,9157,9538], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2150),elname="xbrick",eltype="xbrick",typekey=2150) 

        call prepare(lib_xbrick(2151),key=2151, & 
& nodecnc=[1745,1872,320,1994,4317,4444,2892,4566,23526, 23525,9352, 9351,22878, 22877,24265, 24266,23528 & 
& , 23527,9360, 9359,22880, 22879,24267, 24268], & 
& edgecnc=[9191,2104,8867,9561,9192,2108,8868,9562], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2151),elname="xbrick",eltype="xbrick",typekey=2151) 

        call prepare(lib_xbrick(2152),key=2152, & 
& nodecnc=[167,2013,1986,2041,2739,4585,4558,4613,24269, 24270,23638, 23637,7982, 7981,24271, 24272,24273 & 
& , 24274,23640, 23639,7988, 7987,24275, 24276], & 
& edgecnc=[9563,9247,1419,9564,9565,9248,1422,9566], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2152),elname="xbrick",eltype="xbrick",typekey=2152) 

        call prepare(lib_xbrick(2153),key=2153, & 
& nodecnc=[2118,2042,113,1988,4690,4614,2685,4560,5800, 5799,10208, 10207,23570, 23569,24277, 24278,5808 & 
& , 5807,10214, 10213,23574, 23573,24279, 24280], & 
& edgecnc=[328,2532,9213,9567,332,2535,9215,9568], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2153),elname="xbrick",eltype="xbrick",typekey=2153) 

        call prepare(lib_xbrick(2154),key=2154, & 
& nodecnc=[114,1998,2045,2499,2686,4570,4617,5071,24140, 24139,24281, 24282,24283, 24284,10220, 10219 & 
& ,24144, 24143,24285, 24286,24287, 24288,10228, 10227], & 
& edgecnc=[9498,9569,9570,2538,9500,9571,9572,2542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2154),elname="xbrick",eltype="xbrick",typekey=2154) 

        call prepare(lib_xbrick(2155),key=2155, & 
& nodecnc=[2386,1989,1852,284,4958,4561,4424,2856,24289, 24290,23290, 23289,23206, 23205,10250, 10249 & 
& ,24291, 24292,23294, 23293,23212, 23211,10258, 10257], & 
& edgecnc=[9573,9073,9031,2553,9574,9075,9034,2557], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2155),elname="xbrick",eltype="xbrick",typekey=2155) 

        call prepare(lib_xbrick(2156),key=2156, & 
& nodecnc=[2376,1990,1851,298,4948,4562,4423,2870,24086, 24085,23270, 23269,23194, 23193,8006, 8005,24090 & 
& , 24089,23274, 23273,23200, 23199,8014, 8013], & 
& edgecnc=[9471,9063,9025,1431,9473,9065,9028,1435], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2156),elname="xbrick",eltype="xbrick",typekey=2156) 

        call prepare(lib_xbrick(2157),key=2157, & 
& nodecnc=[1997,1992,1862,245,4569,4564,4434,2817,22648, 22647,9948, 9947,23160, 23159,22186, 22185,22652 & 
& , 22651,9956, 9955,23164, 23163,22190, 22189], & 
& edgecnc=[8752,2402,9008,8521,8754,2406,9010,8523], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2157),elname="xbrick",eltype="xbrick",typekey=2157) 

        call prepare(lib_xbrick(2158),key=2158, & 
& nodecnc=[1958,1993,2355,296,4530,4565,4927,2868,23792, 23791,10536, 10535,23660, 23659,12652, 12651 & 
& ,23796, 23795,10544, 10543,23664, 23663,12660, 12659], & 
& edgecnc=[9324,2696,9258,3754,9326,2700,9260,3758], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2158),elname="xbrick",eltype="xbrick",typekey=2158) 

        call prepare(lib_xbrick(2159),key=2159, & 
& nodecnc=[1770,1813,114,1995,4342,4385,2686,4567,22914, 22913,23384, 23383,10218, 10217,23144, 23143 & 
& ,22916, 22915,23388, 23387,10226, 10225,23148, 23147], & 
& edgecnc=[8885,9120,2537,9000,8886,9122,2541,9002], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2159),elname="xbrick",eltype="xbrick",typekey=2159) 

        call prepare(lib_xbrick(2160),key=2160, & 
& nodecnc=[199,1996,1931,2043,2771,4568,4503,4615,24293, 24294,22728, 22727,10008, 10007,9294, 9293,24295 & 
& , 24296,22736, 22735,10016, 10015,9302, 9301], & 
& edgecnc=[9575,8792,2432,2075,9576,8796,2436,2079], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2160),elname="xbrick",eltype="xbrick",typekey=2160) 

        call prepare(lib_xbrick(2161),key=2161, & 
& nodecnc=[2118,1988,2499,2045,4690,4560,5071,4617,24278, 24277,10222, 10221,24284, 24283,24297, 24298 & 
& ,24280, 24279,10230, 10229,24288, 24287,24299, 24300], & 
& edgecnc=[9567,2539,9570,9577,9568,2543,9572,9578], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2161),elname="xbrick",eltype="xbrick",typekey=2161) 

        call prepare(lib_xbrick(2162),key=2162, & 
& nodecnc=[196,2045,1998,2504,2768,4617,4570,5076,24301, 24302,24282, 24281,23394, 23393,24098, 24097 & 
& ,24303, 24304,24286, 24285,23400, 23399,24104, 24103], & 
& edgecnc=[9579,9569,9125,9477,9580,9571,9128,9480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2162),elname="xbrick",eltype="xbrick",typekey=2162) 

        call prepare(lib_xbrick(2163),key=2163, & 
& nodecnc=[1780,1838,285,1999,4352,4410,2857,4571,5766, 5765,24305, 24306,24307, 24308,23208, 23207,5774 & 
& , 5773,24309, 24310,24311, 24312,23214, 23213], & 
& edgecnc=[311,9581,9582,9032,315,9583,9584,9035], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2163),elname="xbrick",eltype="xbrick",typekey=2163) 

        call prepare(lib_xbrick(2164),key=2164, & 
& nodecnc=[1779,1837,297,2000,4351,4409,2869,4572,8022, 8021,22974, 22973,24313, 24314,23196, 23195,8030 & 
& , 8029,22980, 22979,24315, 24316,23202, 23201], & 
& edgecnc=[1439,8915,9585,9026,1443,8918,9586,9029], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2164),elname="xbrick",eltype="xbrick",typekey=2164) 

        call prepare(lib_xbrick(2165),key=2165, & 
& nodecnc=[2156,117,2113,1707,4728,2689,4685,4279,24317, 24318,22490, 22489,22714, 22713,24319, 24320 & 
& ,24321, 24322,22496, 22495,22718, 22717,24323, 24324], & 
& edgecnc=[9587,8673,8785,9588,9589,8676,8787,9590], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2165),elname="xbrick",eltype="xbrick",typekey=2165) 

        call prepare(lib_xbrick(2166),key=2166, & 
& nodecnc=[1959,2474,282,1879,4531,5046,2854,4451,24325, 24326,9270, 9269,23314, 23313,24124, 24123,24327 & 
& , 24328,9278, 9277,23320, 23319,24128, 24127], & 
& edgecnc=[9591,2063,9085,9490,9592,2067,9088,9492], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2166),elname="xbrick",eltype="xbrick",typekey=2166) 

        call prepare(lib_xbrick(2167),key=2167, & 
& nodecnc=[1940,300,1878,2465,4512,2872,4450,5037,23946, 23945,23300, 23299,23588, 23587,10768, 10767 & 
& ,23948, 23947,23304, 23303,23592, 23591,10776, 10775], & 
& edgecnc=[9401,9078,9222,2812,9402,9080,9224,2816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2167),elname="xbrick",eltype="xbrick",typekey=2167) 

        call prepare(lib_xbrick(2168),key=2168, & 
& nodecnc=[1961,263,1821,2002,4533,2835,4393,4574,24329, 24330,23654, 23653,22962, 22961,23736, 23735 & 
& ,24331, 24332,23656, 23655,22964, 22963,23742, 23741], & 
& edgecnc=[9593,9255,8909,9296,9594,9256,8910,9299], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2168),elname="xbrick",eltype="xbrick",typekey=2168) 

        call prepare(lib_xbrick(2169),key=2169, & 
& nodecnc=[2079,2116,263,1961,4651,4688,2835,4533,24333, 24334,10606, 10605,24330, 24329,24132, 24131 & 
& ,24335, 24336,10614, 10613,24332, 24331,24136, 24135], & 
& edgecnc=[9595,2731,9593,9494,9596,2735,9594,9496], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2169),elname="xbrick",eltype="xbrick",typekey=2169) 

        call prepare(lib_xbrick(2170),key=2170, & 
& nodecnc=[2534,2003,2494,1919,5106,4575,5066,4491,23580, 23579,24337, 24338,10234, 10233,10324, 10323 & 
& ,23584, 23583,24339, 24340,10242, 10241,10330, 10329], & 
& edgecnc=[9218,9597,2545,2590,9220,9598,2549,2593], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2170),elname="xbrick",eltype="xbrick",typekey=2170) 

        call prepare(lib_xbrick(2171),key=2171, & 
& nodecnc=[2040,2073,2494,2003,4612,4645,5066,4575,9242, 9241,24341, 24342,24338, 24337,24343, 24344,9250 & 
& , 9249,24345, 24346,24340, 24339,24347, 24348], & 
& edgecnc=[2049,9599,9597,9600,2053,9601,9598,9602], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2171),elname="xbrick",eltype="xbrick",typekey=2171) 

        call prepare(lib_xbrick(2172),key=2172, & 
& nodecnc=[2144,2091,2097,2498,4716,4663,4669,5070,24349, 24350,9608, 9607,24351, 24352,24353, 24354,24355 & 
& , 24356,9616, 9615,24357, 24358,24359, 24360], & 
& edgecnc=[9603,2232,9604,9605,9606,2236,9607,9608], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2172),elname="xbrick",eltype="xbrick",typekey=2172) 

        call prepare(lib_xbrick(2173),key=2173, & 
& nodecnc=[2102,2050,2278,108,4674,4622,4850,2680,23904, 23903,24230, 24229,23682, 23681,24361, 24362 & 
& ,23908, 23907,24234, 24233,23688, 23687,24363, 24364], & 
& edgecnc=[9380,9543,9269,9609,9382,9545,9272,9610], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2173),elname="xbrick",eltype="xbrick",typekey=2173) 

        call prepare(lib_xbrick(2174),key=2174, & 
& nodecnc=[2086,154,2071,2112,4658,2726,4643,4684,10168, 10167,24365, 24366,5834, 5833,24367, 24368,10174 & 
& , 10173,24369, 24370,5842, 5841,24371, 24372], & 
& edgecnc=[2512,9611,345,9612,2515,9613,349,9614], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2174),elname="xbrick",eltype="xbrick",typekey=2174) 

        call prepare(lib_xbrick(2175),key=2175, & 
& nodecnc=[2070,2136,2069,184,4642,4708,4641,2756,24373, 24374,24375, 24376,10590, 10589,11746, 11745 & 
& ,24377, 24378,24379, 24380,10596, 10595,11752, 11751], & 
& edgecnc=[9615,9616,2723,3301,9617,9618,2726,3304], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2175),elname="xbrick",eltype="xbrick",typekey=2175) 

        call prepare(lib_xbrick(2176),key=2176, & 
& nodecnc=[2014,2484,2411,2039,4586,5056,4983,4611,9668, 9667,6126, 6125,24156, 24155,12550, 12549,9672 & 
& , 9671,6134, 6133,24160, 24159,12556, 12555], & 
& edgecnc=[2262,491,9506,3703,2264,495,9508,3706], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2176),elname="xbrick",eltype="xbrick",typekey=2176) 

        call prepare(lib_xbrick(2177),key=2177, & 
& nodecnc=[2326,2016,2035,222,4898,4588,4607,2794,24381, 24382,12148, 12147,6896, 6895,24383, 24384,24385 & 
& , 24386,12156, 12155,6904, 6903,24387, 24388], & 
& edgecnc=[9619,3502,876,9620,9621,3506,880,9622], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2177),elname="xbrick",eltype="xbrick",typekey=2177) 

        call prepare(lib_xbrick(2178),key=2178, & 
& nodecnc=[2038,2010,1967,2326,4610,4582,4539,4898,7446, 7445,23838, 23837,24389, 24390,24391, 24392,7454 & 
& , 7453,23842, 23841,24393, 24394,24395, 24396], & 
& edgecnc=[1151,9347,9623,9624,1155,9349,9625,9626], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2178),elname="xbrick",eltype="xbrick",typekey=2178) 

        call prepare(lib_xbrick(2179),key=2179, & 
& nodecnc=[2345,2344,1966,2033,4917,4916,4538,4605,24397, 24398,24174, 24173,12910, 12909,9014, 9013,24399 & 
& , 24400,24178, 24177,12918, 12917,9022, 9021], & 
& edgecnc=[9627,9515,3883,1935,9628,9517,3887,1939], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2179),elname="xbrick",eltype="xbrick",typekey=2179) 

        call prepare(lib_xbrick(2180),key=2180, & 
& nodecnc=[2111,2063,1977,2134,4683,4635,4549,4706,9396, 9395,24214, 24213,9172, 9171,22326, 22325,9402 & 
& , 9401,24218, 24217,9180, 9179,22328, 22327], & 
& edgecnc=[2126,9535,2014,8591,2129,9537,2018,8592], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2180),elname="xbrick",eltype="xbrick",typekey=2180) 

        call prepare(lib_xbrick(2181),key=2181, & 
& nodecnc=[264,1943,2382,2044,2836,4515,4954,4616,24210, 24209,24401, 24402,24403, 24404,23974, 23973 & 
& ,24212, 24211,24405, 24406,24407, 24408,23978, 23977], & 
& edgecnc=[9533,9629,9630,9415,9534,9631,9632,9417], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2181),elname="xbrick",eltype="xbrick",typekey=2181) 

        call prepare(lib_xbrick(2182),key=2182, & 
& nodecnc=[2110,130,1918,2026,4682,2702,4490,4598,24409, 24410,23546, 23545,10782, 10781,10698, 10697 & 
& ,24411, 24412,23552, 23551,10790, 10789,10706, 10705], & 
& edgecnc=[9633,9201,2819,2777,9634,9204,2823,2781], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2182),elname="xbrick",eltype="xbrick",typekey=2182) 

        call prepare(lib_xbrick(2183),key=2183, & 
& nodecnc=[2119,200,2021,2075,4691,2772,4593,4647,24413, 24414,24415, 24416,24417, 24418,24114, 24113 & 
& ,24419, 24420,24421, 24422,24423, 24424,24120, 24119], & 
& edgecnc=[9635,9636,9637,9485,9638,9639,9640,9488], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2183),elname="xbrick",eltype="xbrick",typekey=2183) 

        call prepare(lib_xbrick(2184),key=2184, & 
& nodecnc=[2075,2021,2403,2509,4647,4593,4975,5081,24418, 24417,23986, 23985,9266, 9265,9286, 9285,24424 & 
& , 24423,23990, 23989,9274, 9273,9292, 9291], & 
& edgecnc=[9637,9421,2061,2071,9640,9423,2065,2074], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2184),elname="xbrick",eltype="xbrick",typekey=2184) 

        call prepare(lib_xbrick(2185),key=2185, & 
& nodecnc=[129,2158,2376,2022,2701,4730,4948,4594,10752, 10751,24088, 24087,8012, 8011,10736, 10735,10758 & 
& , 10757,24092, 24091,8020, 8019,10744, 10743], & 
& edgecnc=[2804,9472,1434,2796,2807,9474,1438,2800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2185),elname="xbrick",eltype="xbrick",typekey=2185) 

        call prepare(lib_xbrick(2186),key=2186, & 
& nodecnc=[2148,129,2141,2140,4720,2701,4713,4712,10754, 10753,10734, 10733,24425, 24426,24427, 24428 & 
& ,10760, 10759,10742, 10741,24429, 24430,24431, 24432], & 
& edgecnc=[2805,2795,9641,9642,2808,2799,9643,9644], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2186),elname="xbrick",eltype="xbrick",typekey=2186) 

        call prepare(lib_xbrick(2187),key=2187, & 
& nodecnc=[1989,2386,2427,2154,4561,4958,4999,4726,24290, 24289,24433, 24434,10302, 10301,23620, 23619 & 
& ,24292, 24291,24435, 24436,10308, 10307,23624, 23623], & 
& edgecnc=[9573,9645,2579,9238,9574,9646,2582,9240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2187),elname="xbrick",eltype="xbrick",typekey=2187) 

        call prepare(lib_xbrick(2188),key=2188, & 
& nodecnc=[1982,198,2023,2055,4554,2770,4595,4627,10238, 10237,24437, 24438,10314, 10313,24254, 24253 & 
& ,10246, 10245,24439, 24440,10320, 10319,24256, 24255], & 
& edgecnc=[2547,9647,2585,9555,2551,9648,2588,9556], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2188),elname="xbrick",eltype="xbrick",typekey=2188) 

        call prepare(lib_xbrick(2189),key=2189, & 
& nodecnc=[2137,2024,1968,2470,4709,4596,4540,5042,24441, 24442,21562, 21561,24186, 24185,22488, 22487 & 
& ,24443, 24444,21564, 21563,24188, 24187,22494, 22493], & 
& edgecnc=[9649,8209,9521,8672,9650,8210,9522,8675], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2189),elname="xbrick",eltype="xbrick",typekey=2189) 

        call prepare(lib_xbrick(2190),key=2190, & 
& nodecnc=[1895,301,2025,2471,4467,2873,4597,5043,22938, 22937,23756, 23755,23478, 23477,22446, 22445 & 
& ,22944, 22943,23760, 23759,23486, 23485,22452, 22451], & 
& edgecnc=[8897,9306,9167,8651,8900,9308,9171,8654], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2190),elname="xbrick",eltype="xbrick",typekey=2190) 

        call prepare(lib_xbrick(2191),key=2191, & 
& nodecnc=[2533,130,2110,2106,5105,2702,4682,4678,24445, 24446,24410, 24409,24080, 24079,10690, 10689 & 
& ,24447, 24448,24412, 24411,24084, 24083,10696, 10695], & 
& edgecnc=[9651,9633,9468,2773,9652,9634,9470,2776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2191),elname="xbrick",eltype="xbrick",typekey=2191) 

        call prepare(lib_xbrick(2192),key=2192, & 
& nodecnc=[2027,2493,2467,1955,4599,5065,5039,4527,24110, 24109,24449, 24450,23610, 23609,24451, 24452 & 
& ,24116, 24115,24453, 24454,23616, 23615,24455, 24456], & 
& edgecnc=[9483,9653,9233,9654,9486,9655,9236,9656], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2192),elname="xbrick",eltype="xbrick",typekey=2192) 

        call prepare(lib_xbrick(2193),key=2193, & 
& nodecnc=[2028,2077,2000,297,4600,4649,4572,2869,10658, 10657,23816, 23815,24314, 24313,23778, 23777 & 
& ,10666, 10665,23820, 23819,24316, 24315,23784, 23783], & 
& edgecnc=[2757,9336,9585,9317,2761,9338,9586,9320], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2193),elname="xbrick",eltype="xbrick",typekey=2193) 

        call prepare(lib_xbrick(2194),key=2194, & 
& nodecnc=[2383,2019,218,2382,4955,4591,2790,4954,23470, 23469,24168, 24167,24457, 24458,24459, 24460 & 
& ,23474, 23473,24172, 24171,24461, 24462,24463, 24464], & 
& edgecnc=[9163,9512,9657,9658,9165,9514,9659,9660], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2194),elname="xbrick",eltype="xbrick",typekey=2194) 

        call prepare(lib_xbrick(2195),key=2195, & 
& nodecnc=[2155,219,2116,2079,4727,2791,4688,4651,24465, 24466,10608, 10607,24334, 24333,24467, 24468 & 
& ,24469, 24470,10616, 10615,24336, 24335,24471, 24472], & 
& edgecnc=[9661,2732,9595,9662,9663,2736,9596,9664], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2195),elname="xbrick",eltype="xbrick",typekey=2195) 

        call prepare(lib_xbrick(2196),key=2196, & 
& nodecnc=[1941,2458,2418,2076,4513,5030,4990,4648,23822, 23821,24473, 24474,24475, 24476,10310, 10309 & 
& ,23826, 23825,24477, 24478,24479, 24480,10316, 10315], & 
& edgecnc=[9339,9665,9666,2583,9341,9667,9668,2586], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2196),elname="xbrick",eltype="xbrick",typekey=2196) 

        call prepare(lib_xbrick(2197),key=2197, & 
& nodecnc=[2523,197,2083,2062,5095,2769,4655,4634,24246, 24245,24481, 24482,24094, 24093,8984, 8983,24250 & 
& , 24249,24483, 24484,24100, 24099,8992, 8991], & 
& edgecnc=[9551,9669,9475,1920,9553,9670,9478,1924], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2197),elname="xbrick",eltype="xbrick",typekey=2197) 

        call prepare(lib_xbrick(2198),key=2198, & 
& nodecnc=[196,2084,2118,2045,2768,4656,4690,4617,11984, 11983,24485, 24486,24298, 24297,24302, 24301 & 
& ,11992, 11991,24487, 24488,24300, 24299,24304, 24303], & 
& edgecnc=[3420,9671,9577,9579,3424,9672,9578,9580], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2198),elname="xbrick",eltype="xbrick",typekey=2198) 

        call prepare(lib_xbrick(2199),key=2199, & 
& nodecnc=[1951,2046,2071,2120,4523,4618,4643,4692,12894, 12893,24489, 24490,24491, 24492,9010, 9009,12902 & 
& , 12901,24493, 24494,24495, 24496,9018, 9017], & 
& edgecnc=[3875,9673,9674,1933,3879,9675,9676,1937], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2199),elname="xbrick",eltype="xbrick",typekey=2199) 

        call prepare(lib_xbrick(2200),key=2200, & 
& nodecnc=[2128,1949,2034,2086,4700,4521,4606,4658,24030, 24029,24026, 24025,10170, 10169,24497, 24498 & 
& ,24036, 24035,24028, 24027,10176, 10175,24499, 24500], & 
& edgecnc=[9443,9441,2513,9677,9446,9442,2516,9678], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2200),elname="xbrick",eltype="xbrick",typekey=2200) 

        call prepare(lib_xbrick(2201),key=2201, & 
& nodecnc=[2325,2147,2145,2087,4897,4719,4717,4659,24501, 24502,24503, 24504,6900, 6899,24505, 24506,24507 & 
& , 24508,24509, 24510,6908, 6907,24511, 24512], & 
& edgecnc=[9679,9680,878,9681,9682,9683,882,9684], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2201),elname="xbrick",eltype="xbrick",typekey=2201) 

        call prepare(lib_xbrick(2202),key=2202, & 
& nodecnc=[2015,2088,2361,2037,4587,4660,4933,4609,7042, 7041,10476, 10475,24513, 24514,24006, 24005,7048 & 
& , 7047,10484, 10483,24515, 24516,24010, 24009], & 
& edgecnc=[949,2666,9685,9431,952,2670,9686,9433], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2202),elname="xbrick",eltype="xbrick",typekey=2202) 

        call prepare(lib_xbrick(2203),key=2203, & 
& nodecnc=[2091,2144,193,2484,4663,4716,2765,5056,24350, 24349,24517, 24518,6128, 6127,9666, 9665,24356 & 
& , 24355,24519, 24520,6136, 6135,9670, 9669], & 
& edgecnc=[9603,9687,492,2261,9606,9688,496,2263], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2203),elname="xbrick",eltype="xbrick",typekey=2203) 

        call prepare(lib_xbrick(2204),key=2204, & 
& nodecnc=[2144,2151,2092,193,4716,4723,4664,2765,24521, 24522,24523, 24524,6122, 6121,24518, 24517,24525 & 
& , 24526,24527, 24528,6130, 6129,24520, 24519], & 
& edgecnc=[9689,9690,489,9687,9691,9692,493,9688], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2204),elname="xbrick",eltype="xbrick",typekey=2204) 

        call prepare(lib_xbrick(2205),key=2205, & 
& nodecnc=[2494,2073,2427,198,5066,4645,4999,2770,24342, 24341,10298, 10297,24529, 24530,10236, 10235 & 
& ,24346, 24345,10304, 10303,24531, 24532,10244, 10243], & 
& edgecnc=[9599,2577,9693,2546,9601,2580,9694,2550], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2205),elname="xbrick",eltype="xbrick",typekey=2205) 

        call prepare(lib_xbrick(2206),key=2206, & 
& nodecnc=[199,2040,2003,1996,2771,4612,4575,4568,9244, 9243,24344, 24343,23578, 23577,24294, 24293,9252 & 
& , 9251,24348, 24347,23582, 23581,24296, 24295], & 
& edgecnc=[2050,9600,9217,9575,2054,9602,9219,9576], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2206),elname="xbrick",eltype="xbrick",typekey=2206) 

        call prepare(lib_xbrick(2207),key=2207, & 
& nodecnc=[2551,167,2041,2109,5123,2739,4613,4681,24533, 24534,24272, 24271,11728, 11727,24535, 24536 & 
& ,24537, 24538,24276, 24275,11736, 11735,24539, 24540], & 
& edgecnc=[9695,9564,3292,9696,9697,9566,3296,9698], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2207),elname="xbrick",eltype="xbrick",typekey=2207) 

        call prepare(lib_xbrick(2208),key=2208, & 
& nodecnc=[1754,166,2041,2153,4326,2738,4613,4725,22920, 22919,11730, 11729,7980, 7979,22584, 22583,22926 & 
& , 22925,11738, 11737,7986, 7985,22588, 22587], & 
& edgecnc=[8888,3293,1418,8720,8891,3297,1421,8722], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2208),elname="xbrick",eltype="xbrick",typekey=2208) 

        call prepare(lib_xbrick(2209),key=2209, & 
& nodecnc=[2112,2012,106,2188,4684,4584,2678,4760,5832, 5831,24192, 24191,24541, 24542,24543, 24544,5840 & 
& , 5839,24196, 24195,24545, 24546,24547, 24548], & 
& edgecnc=[344,9524,9699,9700,348,9526,9701,9702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2209),elname="xbrick",eltype="xbrick",typekey=2209) 

        call prepare(lib_xbrick(2210),key=2210, & 
& nodecnc=[2068,106,1913,1991,4640,2678,4485,4563,24549, 24550,24190, 24189,5820, 5819,10156, 10155,24551 & 
& , 24552,24194, 24193,5828, 5827,10164, 10163], & 
& edgecnc=[9703,9523,338,2506,9704,9525,342,2510], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2210),elname="xbrick",eltype="xbrick",typekey=2210) 

        call prepare(lib_xbrick(2211),key=2211, & 
& nodecnc=[2382,218,2421,2044,4954,2790,4993,4616,24458, 24457,24553, 24554,10622, 10621,24404, 24403 & 
& ,24462, 24461,24555, 24556,10630, 10629,24408, 24407], & 
& edgecnc=[9657,9705,2739,9630,9659,9706,2743,9632], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2211),elname="xbrick",eltype="xbrick",typekey=2211) 

        call prepare(lib_xbrick(2212),key=2212, & 
& nodecnc=[2071,2046,2001,153,4643,4618,4573,2725,24490, 24489,24050, 24049,23362, 23361,5836, 5835,24494 & 
& , 24493,24054, 24053,23368, 23367,5844, 5843], & 
& edgecnc=[9673,9453,9109,346,9675,9455,9112,350], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2212),elname="xbrick",eltype="xbrick",typekey=2212) 

        call prepare(lib_xbrick(2213),key=2213, & 
& nodecnc=[2393,2121,2095,2048,4965,4693,4667,4620,24557, 24558,24559, 24560,12628, 12627,24561, 24562 & 
& ,24563, 24564,24565, 24566,12632, 12631,24567, 24568], & 
& edgecnc=[9707,9708,3742,9709,9710,9711,3744,9712], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2213),elname="xbrick",eltype="xbrick",typekey=2213) 

        call prepare(lib_xbrick(2214),key=2214, & 
& nodecnc=[2088,2152,126,2094,4660,4724,2698,4666,7040, 7039,24569, 24570,24571, 24572,10478, 10477,7046 & 
& , 7045,24573, 24574,24575, 24576,10486, 10485], & 
& edgecnc=[948,9713,9714,2667,951,9715,9716,2671], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2214),elname="xbrick",eltype="xbrick",typekey=2214) 

        call prepare(lib_xbrick(2215),key=2215, & 
& nodecnc=[2103,2122,171,2049,4675,4694,2743,4621,24014, 24013,24577, 24578,24579, 24580,6910, 6909,24020 & 
& , 24019,24581, 24582,24583, 24584,6918, 6917], & 
& edgecnc=[9435,9717,9718,883,9438,9719,9720,887], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2215),elname="xbrick",eltype="xbrick",typekey=2215) 

        call prepare(lib_xbrick(2216),key=2216, & 
& nodecnc=[2166,111,2051,2123,4738,2683,4623,4695,24585, 24586,24587, 24588,5934, 5933,24589, 24590,24591 & 
& , 24592,24593, 24594,5942, 5941,24595, 24596], & 
& edgecnc=[9721,9722,395,9723,9724,9725,399,9726], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2216),elname="xbrick",eltype="xbrick",typekey=2216) 

        call prepare(lib_xbrick(2217),key=2217, & 
& nodecnc=[2396,2124,2100,2053,4968,4696,4672,4625,24597, 24598,24599, 24600,6116, 6115,24601, 24602,24603 & 
& , 24604,24605, 24606,6120, 6119,24607, 24608], & 
& edgecnc=[9727,9728,486,9729,9730,9731,488,9732], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2217),elname="xbrick",eltype="xbrick",typekey=2217) 

        call prepare(lib_xbrick(2218),key=2218, & 
& nodecnc=[2151,194,2099,2092,4723,2766,4671,4664,24609, 24610,24611, 24612,23998, 23997,24524, 24523 & 
& ,24613, 24614,24615, 24616,24004, 24003,24528, 24527], & 
& edgecnc=[9733,9734,9427,9690,9735,9736,9430,9692], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2218),elname="xbrick",eltype="xbrick",typekey=2218) 

        call prepare(lib_xbrick(2219),key=2219, & 
& nodecnc=[1643,101,2054,2512,4215,2673,4626,5084,22408, 22407,21320, 21319,21218, 21217,8910, 8909,22416 & 
& , 22415,21324, 21323,21220, 21219,8918, 8917], & 
& edgecnc=[8632,8088,8037,1883,8636,8090,8038,1887], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2219),elname="xbrick",eltype="xbrick",typekey=2219) 

        call prepare(lib_xbrick(2220),key=2220, & 
& nodecnc=[2418,2083,197,2076,4990,4655,2769,4648,23798, 23797,24482, 24481,23956, 23955,24476, 24475 & 
& ,23802, 23801,24484, 24483,23962, 23961,24480, 24479], & 
& edgecnc=[9327,9669,9406,9666,9329,9670,9409,9668], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2220),elname="xbrick",eltype="xbrick",typekey=2220) 

        call prepare(lib_xbrick(2221),key=2221, & 
& nodecnc=[2161,2193,2056,2085,4733,4765,4628,4657,24617, 24618,24619, 24620,10210, 10209,24621, 24622 & 
& ,24623, 24624,24625, 24626,10216, 10215,24627, 24628], & 
& edgecnc=[9737,9738,2533,9739,9740,9741,2536,9742], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2221),elname="xbrick",eltype="xbrick",typekey=2221) 

        call prepare(lib_xbrick(2222),key=2222, & 
& nodecnc=[200,2119,2117,2129,2772,4691,4689,4701,24414, 24413,9298, 9297,24629, 24630,24631, 24632,24420 & 
& , 24419,9306, 9305,24633, 24634,24635, 24636], & 
& edgecnc=[9635,2077,9743,9744,9638,2081,9745,9746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2222),elname="xbrick",eltype="xbrick",typekey=2222) 

        call prepare(lib_xbrick(2223),key=2223, & 
& nodecnc=[2104,2059,1978,227,4676,4631,4550,2799,7118, 7117,24222, 24221,7144, 7143,13466, 13465,7124 & 
& , 7123,24224, 24223,7152, 7151,13472, 13471], & 
& edgecnc=[987,9539,1000,4161,990,9540,1004,4164], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2223),elname="xbrick",eltype="xbrick",typekey=2223) 

        call prepare(lib_xbrick(2224),key=2224, & 
& nodecnc=[2485,2093,2057,201,5057,4665,4629,2773,8930, 8929,22702, 22701,24240, 24239,24262, 24261,8938 & 
& , 8937,22710, 22709,24244, 24243,24264, 24263], & 
& edgecnc=[1893,8779,9548,9559,1897,8783,9550,9560], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2224),elname="xbrick",eltype="xbrick",typekey=2224) 

        call prepare(lib_xbrick(2225),key=2225, & 
& nodecnc=[1957,2032,196,2083,4529,4604,2768,4655,24058, 24057,11986, 11985,24096, 24095,23800, 23799 & 
& ,24060, 24059,11994, 11993,24102, 24101,23804, 23803], & 
& edgecnc=[9457,3421,9476,9328,9458,3425,9479,9330], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2225),elname="xbrick",eltype="xbrick",typekey=2225) 

        call prepare(lib_xbrick(2226),key=2226, & 
& nodecnc=[2154,2067,2027,1955,4726,4639,4599,4527,10300, 10299,24637, 24638,24452, 24451,23618, 23617 & 
& ,10306, 10305,24639, 24640,24456, 24455,23622, 23621], & 
& edgecnc=[2578,9747,9654,9237,2581,9748,9656,9239], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2226),elname="xbrick",eltype="xbrick",typekey=2226) 

        call prepare(lib_xbrick(2227),key=2227, & 
& nodecnc=[2067,199,2119,2027,4639,2771,4691,4599,9238, 9237,9300, 9299,24112, 24111,24638, 24637,9246 & 
& , 9245,9308, 9307,24118, 24117,24640, 24639], & 
& edgecnc=[2047,2078,9484,9747,2051,2082,9487,9748], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2227),elname="xbrick",eltype="xbrick",typekey=2227) 

        call prepare(lib_xbrick(2228),key=2228, & 
& nodecnc=[2123,2472,107,2166,4695,5044,2679,4738,24641, 24642,24643, 24644,24645, 24646,24590, 24589 & 
& ,24647, 24648,24649, 24650,24651, 24652,24596, 24595], & 
& edgecnc=[9749,9750,9751,9723,9752,9753,9754,9726], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2228),elname="xbrick",eltype="xbrick",typekey=2228) 

        call prepare(lib_xbrick(2229),key=2229, & 
& nodecnc=[2184,111,2166,2195,4756,2683,4738,4767,24653, 24654,24586, 24585,24655, 24656,24657, 24658 & 
& ,24659, 24660,24592, 24591,24661, 24662,24663, 24664], & 
& edgecnc=[9755,9721,9756,9757,9758,9724,9759,9760], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2229),elname="xbrick",eltype="xbrick",typekey=2229) 

        call prepare(lib_xbrick(2230),key=2230, & 
& nodecnc=[1944,2021,200,2072,4516,4593,2772,4644,23988, 23987,24416, 24415,24665, 24666,23982, 23981 & 
& ,23992, 23991,24422, 24421,24667, 24668,23984, 23983], & 
& edgecnc=[9422,9636,9761,9419,9424,9639,9762,9420], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2230),elname="xbrick",eltype="xbrick",typekey=2230) 

        call prepare(lib_xbrick(2231),key=2231, & 
& nodecnc=[2386,2023,198,2427,4958,4595,2770,4999,10256, 10255,24438, 24437,24530, 24529,24434, 24433 & 
& ,10264, 10263,24440, 24439,24532, 24531,24436, 24435], & 
& edgecnc=[2556,9647,9693,9645,2560,9648,9694,9646], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2231),elname="xbrick",eltype="xbrick",typekey=2231) 

        call prepare(lib_xbrick(2232),key=2232, & 
& nodecnc=[2489,2022,1942,2074,5061,4594,4514,4646,10738, 10737,8010, 8009,23968, 23967,24669, 24670,10746 & 
& , 10745,8018, 8017,23972, 23971,24671, 24672], & 
& edgecnc=[2797,1433,9412,9763,2801,1437,9414,9764], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2232),elname="xbrick",eltype="xbrick",typekey=2232) 

        call prepare(lib_xbrick(2233),key=2233, & 
& nodecnc=[2422,2421,218,2130,4994,4993,2790,4702,10624, 10623,24554, 24553,24166, 24165,24673, 24674 & 
& ,10632, 10631,24556, 24555,24170, 24169,24675, 24676], & 
& edgecnc=[2740,9705,9511,9765,2744,9706,9513,9766], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2233),elname="xbrick",eltype="xbrick",typekey=2233) 

        call prepare(lib_xbrick(2234),key=2234, & 
& nodecnc=[2081,2125,2121,127,4653,4697,4693,2699,24074, 24073,24677, 24678,24679, 24680,24681, 24682 & 
& ,24076, 24075,24683, 24684,24685, 24686,24687, 24688], & 
& edgecnc=[9465,9767,9768,9769,9466,9770,9771,9772], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2234),elname="xbrick",eltype="xbrick",typekey=2234) 

        call prepare(lib_xbrick(2235),key=2235, & 
& nodecnc=[2131,2238,262,1954,4703,4810,2834,4526,24689, 24690,24691, 24692,24206, 24205,7924, 7923,24693 & 
& , 24694,24695, 24696,24208, 24207,7932, 7931], & 
& edgecnc=[9773,9774,9531,1390,9775,9776,9532,1394], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2235),elname="xbrick",eltype="xbrick",typekey=2235) 

        call prepare(lib_xbrick(2236),key=2236, & 
& nodecnc=[2131,2082,2150,220,4703,4654,4722,2792,7922, 7921,24697, 24698,7904, 7903,24699, 24700,7930 & 
& , 7929,24701, 24702,7912, 7911,24703, 24704], & 
& edgecnc=[1389,9777,1380,9778,1393,9779,1384,9780], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2236),elname="xbrick",eltype="xbrick",typekey=2236) 

        call prepare(lib_xbrick(2237),key=2237, & 
& nodecnc=[2142,195,2127,2124,4714,2767,4699,4696,24705, 24706,24064, 24063,24707, 24708,24709, 24710 & 
& ,24711, 24712,24068, 24067,24713, 24714,24715, 24716], & 
& edgecnc=[9781,9460,9782,9783,9784,9462,9785,9786], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2237),elname="xbrick",eltype="xbrick",typekey=2237) 

        call prepare(lib_xbrick(2238),key=2238, & 
& nodecnc=[112,2203,2193,2161,2684,4775,4765,4733,24717, 24718,24719, 24720,24618, 24617,24721, 24722 & 
& ,24723, 24724,24725, 24726,24624, 24623,24727, 24728], & 
& edgecnc=[9787,9788,9737,9789,9790,9791,9740,9792], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2238),elname="xbrick",eltype="xbrick",typekey=2238) 

        call prepare(lib_xbrick(2239),key=2239, & 
& nodecnc=[2161,2085,2142,2189,4733,4657,4714,4761,24622, 24621,5804, 5803,24729, 24730,24731, 24732,24628 & 
& , 24627,5812, 5811,24733, 24734,24735, 24736], & 
& edgecnc=[9739,330,9793,9794,9742,334,9795,9796], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2239),elname="xbrick",eltype="xbrick",typekey=2239) 

        call prepare(lib_xbrick(2240),key=2240, & 
& nodecnc=[2325,2087,2133,221,4897,4659,4705,2793,24506, 24505,24044, 24043,24737, 24738,11912, 11911 & 
& ,24512, 24511,24048, 24047,24739, 24740,11918, 11917], & 
& edgecnc=[9681,9450,9797,3384,9684,9452,9798,3387], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2240),elname="xbrick",eltype="xbrick",typekey=2240) 

        call prepare(lib_xbrick(2241),key=2241, & 
& nodecnc=[2048,294,1947,2094,4620,2866,4519,4666,12626, 12625,7022, 7021,10480, 10479,24741, 24742,12630 & 
& , 12629,7030, 7029,10488, 10487,24743, 24744], & 
& edgecnc=[3741,939,2668,9799,3743,943,2672,9800], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2241),elname="xbrick",eltype="xbrick",typekey=2241) 

        call prepare(lib_xbrick(2242),key=2242, & 
& nodecnc=[172,2049,171,2096,2744,4621,2743,4668,11946, 11945,24580, 24579,10470, 10469,10458, 10457,11948 & 
& , 11947,24584, 24583,10472, 10471,10464, 10463], & 
& edgecnc=[3401,9718,2663,2657,3402,9720,2664,2660], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2242),elname="xbrick",eltype="xbrick",typekey=2242) 

        call prepare(lib_xbrick(2243),key=2243, & 
& nodecnc=[2126,189,1897,2090,4698,2761,4469,4662,24745, 24746,7464, 7463,23706, 23705,24747, 24748,24749 & 
& , 24750,7472, 7471,23712, 23711,24751, 24752], & 
& edgecnc=[9801,1160,9281,9802,9803,1164,9284,9804], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2243),elname="xbrick",eltype="xbrick",typekey=2243) 

        call prepare(lib_xbrick(2244),key=2244, & 
& nodecnc=[2126,2090,2145,188,4698,4662,4717,2760,24748, 24747,11922, 11921,24753, 24754,24018, 24017 & 
& ,24752, 24751,11928, 11927,24755, 24756,24024, 24023], & 
& edgecnc=[9802,3389,9805,9437,9804,3392,9806,9440], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2244),elname="xbrick",eltype="xbrick",typekey=2244) 

        call prepare(lib_xbrick(2245),key=2245, & 
& nodecnc=[111,2498,2097,2051,2683,5070,4669,4623,24757, 24758,24352, 24351,10202, 10201,24588, 24587 & 
& ,24759, 24760,24358, 24357,10204, 10203,24594, 24593], & 
& edgecnc=[9807,9604,2529,9722,9808,9607,2530,9725], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2245),elname="xbrick",eltype="xbrick",typekey=2245) 

        call prepare(lib_xbrick(2246),key=2246, & 
& nodecnc=[2053,288,1945,2099,4625,2860,4517,4671,6114, 6113,12562, 12561,23994, 23993,24761, 24762,6118 & 
& , 6117,12568, 12567,24000, 23999,24763, 24764], & 
& edgecnc=[485,3709,9425,9809,487,3712,9428,9810], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2246),elname="xbrick",eltype="xbrick",typekey=2246) 

        call prepare(lib_xbrick(2247),key=2247, & 
& nodecnc=[2129,2093,2072,200,4701,4665,4644,2772,22704, 22703,8928, 8927,24666, 24665,24632, 24631,22712 & 
& , 22711,8936, 8935,24668, 24667,24636, 24635], & 
& edgecnc=[8780,1892,9761,9744,8784,1896,9762,9746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2247),elname="xbrick",eltype="xbrick",typekey=2247) 

        call prepare(lib_xbrick(2248),key=2248, & 
& nodecnc=[2393,2048,2094,126,4965,4620,4666,2698,24562, 24561,24742, 24741,24572, 24571,10508, 10507 & 
& ,24568, 24567,24744, 24743,24576, 24575,10516, 10515], & 
& edgecnc=[9709,9799,9714,2682,9712,9800,9716,2686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2248),elname="xbrick",eltype="xbrick",typekey=2248) 

        call prepare(lib_xbrick(2249),key=2249, & 
& nodecnc=[2121,2125,295,2095,4693,4697,2867,4667,24678, 24677,12640, 12639,7858, 7857,24560, 24559,24684 & 
& , 24683,12648, 12647,7866, 7865,24566, 24565], & 
& edgecnc=[9767,3748,1357,9708,9770,3752,1361,9711], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2249),elname="xbrick",eltype="xbrick",typekey=2249) 

        call prepare(lib_xbrick(2250),key=2250, & 
& nodecnc=[2396,2053,2099,194,4968,4625,4671,2766,24602, 24601,24762, 24761,24612, 24611,24765, 24766 & 
& ,24608, 24607,24764, 24763,24616, 24615,24767, 24768], & 
& edgecnc=[9729,9809,9734,9811,9732,9810,9736,9812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2250),elname="xbrick",eltype="xbrick",typekey=2250) 

        call prepare(lib_xbrick(2251),key=2251, & 
& nodecnc=[2124,2127,287,2100,4696,4699,2859,4672,24708, 24707,11972, 11971,5954, 5953,24600, 24599,24714 & 
& , 24713,11980, 11979,5962, 5961,24606, 24605], & 
& edgecnc=[9782,3414,405,9728,9785,3418,409,9731], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2251),elname="xbrick",eltype="xbrick",typekey=2251) 

        call prepare(lib_xbrick(2252),key=2252, & 
& nodecnc=[354,1683,1745,1994,2926,4255,4317,4566,22306, 22305,22868, 22867,24266, 24265,9382, 9381,22308 & 
& , 22307,22874, 22873,24268, 24267,9388, 9387], & 
& edgecnc=[8581,8862,9561,2119,8582,8865,9562,2122], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2252),elname="xbrick",eltype="xbrick",typekey=2252) 

        call prepare(lib_xbrick(2253),key=2253, & 
& nodecnc=[2128,2086,2112,2472,4700,4658,4684,5044,24498, 24497,24368, 24367,24769, 24770,24771, 24772 & 
& ,24500, 24499,24372, 24371,24773, 24774,24775, 24776], & 
& edgecnc=[9677,9612,9813,9814,9678,9614,9815,9816], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2253),elname="xbrick",eltype="xbrick",typekey=2253) 

        call prepare(lib_xbrick(2254),key=2254, & 
& nodecnc=[189,2126,2103,2005,2761,4698,4675,4577,24746, 24745,24016, 24015,6916, 6915,23892, 23891,24750 & 
& , 24749,24022, 24021,6924, 6923,23898, 23897], & 
& edgecnc=[9801,9436,886,9374,9803,9439,890,9377], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2254),elname="xbrick",eltype="xbrick",typekey=2254) 

        call prepare(lib_xbrick(2255),key=2255, & 
& nodecnc=[2105,2148,2140,166,4677,4720,4712,2738,10688, 10687,24428, 24427,11732, 11731,22918, 22917 & 
& ,10694, 10693,24432, 24431,11740, 11739,22924, 22923], & 
& edgecnc=[2772,9642,3294,8887,2775,9644,3298,8890], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2255),elname="xbrick",eltype="xbrick",typekey=2255) 

        call prepare(lib_xbrick(2256),key=2256, & 
& nodecnc=[2564,2551,2109,2135,5136,5123,4681,4707,24777, 24778,24536, 24535,24779, 24780,24781, 24782 & 
& ,24783, 24784,24540, 24539,24785, 24786,24787, 24788], & 
& edgecnc=[9817,9696,9818,9819,9820,9698,9821,9822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2256),elname="xbrick",eltype="xbrick",typekey=2256) 

        call prepare(lib_xbrick(2257),key=2257, & 
& nodecnc=[2140,2141,2135,2109,4712,4713,4707,4681,24426, 24425,7996, 7995,24780, 24779,11726, 11725,24430 & 
& , 24429,8004, 8003,24786, 24785,11734, 11733], & 
& edgecnc=[9641,1426,9818,3291,9643,1430,9821,3295], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2257),elname="xbrick",eltype="xbrick",typekey=2257) 

        call prepare(lib_xbrick(2258),key=2258, & 
& nodecnc=[2195,2166,107,2188,4767,4738,2679,4760,24656, 24655,24646, 24645,24789, 24790,24791, 24792 & 
& ,24662, 24661,24652, 24651,24793, 24794,24795, 24796], & 
& edgecnc=[9756,9751,9823,9824,9759,9754,9825,9826], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2258),elname="xbrick",eltype="xbrick",typekey=2258) 

        call prepare(lib_xbrick(2259),key=2259, & 
& nodecnc=[2157,2114,2138,168,4729,4686,4710,2740,10674, 10673,24797, 24798,24799, 24800,11758, 11757 & 
& ,10680, 10679,24801, 24802,24803, 24804,11764, 11763], & 
& edgecnc=[2765,9827,9828,3307,2768,9829,9830,3310], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2259),elname="xbrick",eltype="xbrick",typekey=2259) 

        call prepare(lib_xbrick(2260),key=2260, & 
& nodecnc=[2138,2114,2013,167,4710,4686,4585,2739,24798, 24797,11744, 11743,24270, 24269,24805, 24806 & 
& ,24802, 24801,11750, 11749,24274, 24273,24807, 24808], & 
& edgecnc=[9827,3300,9563,9831,9829,3303,9565,9832], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2260),elname="xbrick",eltype="xbrick",typekey=2260) 

        call prepare(lib_xbrick(2261),key=2261, & 
& nodecnc=[2115,2237,2139,2160,4687,4809,4711,4732,7126, 7125,7104, 7103,24809, 24810,12162, 12161,7132 & 
& , 7131,7112, 7111,24811, 24812,12170, 12169], & 
& edgecnc=[991,980,9833,3509,994,984,9834,3513], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2261),elname="xbrick",eltype="xbrick",typekey=2261) 

        call prepare(lib_xbrick(2262),key=2262, & 
& nodecnc=[2024,2137,117,2554,4596,4709,2689,5126,24442, 24441,22486, 22485,24813, 24814,10082, 10081 & 
& ,24444, 24443,22492, 22491,24815, 24816,10086, 10085], & 
& edgecnc=[9649,8671,9835,2469,9650,8674,9836,2471], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2262),elname="xbrick",eltype="xbrick",typekey=2262) 

        call prepare(lib_xbrick(2263),key=2263, & 
& nodecnc=[2142,2118,2084,195,4714,4690,4656,2767,5802, 5801,24486, 24485,24062, 24061,24706, 24705,5810 & 
& , 5809,24488, 24487,24066, 24065,24712, 24711], & 
& edgecnc=[329,9671,9459,9781,333,9672,9461,9784], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2263),elname="xbrick",eltype="xbrick",typekey=2263) 

        call prepare(lib_xbrick(2264),key=2264, & 
& nodecnc=[2071,154,2345,2120,4643,2726,4917,4692,24366, 24365,24817, 24818,9012, 9011,24492, 24491,24370 & 
& , 24369,24819, 24820,9020, 9019,24496, 24495], & 
& edgecnc=[9611,9837,1934,9674,9613,9838,1938,9676], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2264),elname="xbrick",eltype="xbrick",typekey=2264) 

        call prepare(lib_xbrick(2265),key=2265, & 
& nodecnc=[2393,2168,127,2121,4965,4740,2699,4693,10506, 10505,7870, 7869,24680, 24679,24558, 24557,10514 & 
& , 10513,7878, 7877,24686, 24685,24564, 24563], & 
& edgecnc=[2681,1363,9768,9707,2685,1367,9771,9710], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2265),elname="xbrick",eltype="xbrick",typekey=2265) 

        call prepare(lib_xbrick(2266),key=2266, & 
& nodecnc=[2123,2387,2128,2472,4695,4959,4700,5044,24821, 24822,24032, 24031,24772, 24771,24642, 24641 & 
& ,24823, 24824,24038, 24037,24776, 24775,24648, 24647], & 
& edgecnc=[9839,9444,9814,9749,9840,9447,9816,9752], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2266),elname="xbrick",eltype="xbrick",typekey=2266) 

        call prepare(lib_xbrick(2267),key=2267, & 
& nodecnc=[2123,2102,108,2387,4695,4674,2680,4959,5940, 5939,24362, 24361,24034, 24033,24822, 24821,5948 & 
& , 5947,24364, 24363,24040, 24039,24824, 24823], & 
& edgecnc=[398,9609,9445,9839,402,9610,9448,9840], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2267),elname="xbrick",eltype="xbrick",typekey=2267) 

        call prepare(lib_xbrick(2268),key=2268, & 
& nodecnc=[2548,2129,2117,2554,5120,4701,4689,5126,22698, 22697,24630, 24629,10084, 10083,24825, 24826 & 
& ,22706, 22705,24634, 24633,10088, 10087,24827, 24828], & 
& edgecnc=[8777,9743,2470,9841,8781,9745,2472,9842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2268),elname="xbrick",eltype="xbrick",typekey=2268) 

        call prepare(lib_xbrick(2269),key=2269, & 
& nodecnc=[2176,2155,2069,2136,4748,4727,4641,4708,24829, 24830,24831, 24832,24376, 24375,24833, 24834 & 
& ,24835, 24836,24837, 24838,24380, 24379,24839, 24840], & 
& edgecnc=[9843,9844,9616,9845,9846,9847,9618,9848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2269),elname="xbrick",eltype="xbrick",typekey=2269) 

        call prepare(lib_xbrick(2270),key=2270, & 
& nodecnc=[2057,2548,2156,2563,4629,5120,4728,5135,22700, 22699,24841, 24842,24843, 24844,23930, 23929 & 
& ,22708, 22707,24845, 24846,24847, 24848,23936, 23935], & 
& edgecnc=[8778,9849,9850,9393,8782,9851,9852,9396], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2270),elname="xbrick",eltype="xbrick",typekey=2270) 

        call prepare(lib_xbrick(2271),key=2271, & 
& nodecnc=[2529,1418,165,2558,5101,3990,2737,5130,10826, 10825,7202, 7201,24849, 24850,24851, 24852,10834 & 
& , 10833,7210, 7209,24853, 24854,24855, 24856], & 
& edgecnc=[2841,1029,9853,9854,2845,1033,9855,9856], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2271),elname="xbrick",eltype="xbrick",typekey=2271) 

        call prepare(lib_xbrick(2272),key=2272, & 
& nodecnc=[2552,1754,1610,2132,5124,4326,4182,4704,22922, 22921,22582, 22581,21618, 21617,24857, 24858 & 
& ,22928, 22927,22586, 22585,21622, 21621,24859, 24860], & 
& edgecnc=[8889,8719,8237,9857,8892,8721,8239,9858], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2272),elname="xbrick",eltype="xbrick",typekey=2272) 

        call prepare(lib_xbrick(2273),key=2273, & 
& nodecnc=[2323,2175,2139,2173,4895,4747,4711,4745,24861, 24862,24863, 24864,7102, 7101,24865, 24866,24867 & 
& , 24868,24869, 24870,7110, 7109,24871, 24872], & 
& edgecnc=[9859,9860,979,9861,9862,9863,983,9864], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2273),elname="xbrick",eltype="xbrick",typekey=2273) 

        call prepare(lib_xbrick(2274),key=2274, & 
& nodecnc=[2136,2159,2150,2176,4708,4731,4722,4748,24873, 24874,11902, 11901,24875, 24876,24834, 24833 & 
& ,24877, 24878,11908, 11907,24879, 24880,24840, 24839], & 
& edgecnc=[9865,3379,9866,9845,9867,3382,9868,9848], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2274),elname="xbrick",eltype="xbrick",typekey=2274) 

        call prepare(lib_xbrick(2275),key=2275, & 
& nodecnc=[2159,2136,2070,185,4731,4708,4642,2757,24874, 24873,24374, 24373,10676, 10675,24881, 24882 & 
& ,24878, 24877,24378, 24377,10682, 10681,24883, 24884], & 
& edgecnc=[9865,9615,2766,9869,9867,9617,2769,9870], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2275),elname="xbrick",eltype="xbrick",typekey=2275) 

        call prepare(lib_xbrick(2276),key=2276, & 
& nodecnc=[2179,2143,128,2169,4751,4715,2700,4741,24885, 24886,24106, 24105,10566, 10565,24887, 24888 & 
& ,24889, 24890,24108, 24107,10572, 10571,24891, 24892], & 
& edgecnc=[9871,9481,2711,9872,9873,9482,2714,9874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2276),elname="xbrick",eltype="xbrick",typekey=2276) 

        call prepare(lib_xbrick(2277),key=2277, & 
& nodecnc=[2138,2179,2169,168,4710,4751,4741,2740,24893, 24894,24888, 24887,24895, 24896,24800, 24799 & 
& ,24897, 24898,24892, 24891,24899, 24900,24804, 24803], & 
& edgecnc=[9875,9872,9876,9828,9877,9874,9878,9830], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2277),elname="xbrick",eltype="xbrick",typekey=2277) 

        call prepare(lib_xbrick(2278),key=2278, & 
& nodecnc=[2175,2177,2160,2139,4747,4749,4732,4711,24901, 24902,24903, 24904,24810, 24809,24864, 24863 & 
& ,24905, 24906,24907, 24908,24812, 24811,24870, 24869], & 
& edgecnc=[9879,9880,9833,9860,9881,9882,9834,9863], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2278),elname="xbrick",eltype="xbrick",typekey=2278) 

        call prepare(lib_xbrick(2279),key=2279, & 
& nodecnc=[220,2173,2238,2131,2792,4745,4810,4703,24909, 24910,7100, 7099,24690, 24689,24700, 24699,24911 & 
& , 24912,7108, 7107,24694, 24693,24704, 24703], & 
& edgecnc=[9883,978,9773,9778,9884,982,9775,9780], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2279),elname="xbrick",eltype="xbrick",typekey=2279) 

        call prepare(lib_xbrick(2280),key=2280, & 
& nodecnc=[2074,2525,2141,2489,4646,5097,4713,5061,24913, 24914,7990, 7989,10740, 10739,24670, 24669,24915 & 
& , 24916,7998, 7997,10748, 10747,24672, 24671], & 
& edgecnc=[9885,1423,2798,9763,9886,1427,2802,9764], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2280),elname="xbrick",eltype="xbrick",typekey=2280) 

        call prepare(lib_xbrick(2281),key=2281, & 
& nodecnc=[194,2161,2189,2396,2766,4733,4761,4968,24917, 24918,24732, 24731,24919, 24920,24766, 24765 & 
& ,24921, 24922,24736, 24735,24923, 24924,24768, 24767], & 
& edgecnc=[9887,9794,9888,9811,9889,9796,9890,9812], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2281),elname="xbrick",eltype="xbrick",typekey=2281) 

        call prepare(lib_xbrick(2282),key=2282, & 
& nodecnc=[188,2145,2147,2163,2760,4717,4719,4735,24754, 24753,24504, 24503,11936, 11935,24925, 24926 & 
& ,24756, 24755,24510, 24509,11942, 11941,24927, 24928], & 
& edgecnc=[9805,9680,3396,9891,9806,9683,3399,9892], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2282),elname="xbrick",eltype="xbrick",typekey=2282) 

        call prepare(lib_xbrick(2283),key=2283, & 
& nodecnc=[2152,2452,2180,126,4724,5024,4752,2698,24929, 24930,24931, 24932,10502, 10501,24570, 24569 & 
& ,24933, 24934,24935, 24936,10510, 10509,24574, 24573], & 
& edgecnc=[9893,9894,2679,9713,9895,9896,2683,9715], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2283),elname="xbrick",eltype="xbrick",typekey=2283) 

        call prepare(lib_xbrick(2284),key=2284, & 
& nodecnc=[127,2172,2149,2081,2699,4744,4721,4653,7876, 7875,11818, 11817,10562, 10561,24682, 24681,7884 & 
& , 7883,11824, 11823,10568, 10567,24688, 24687], & 
& edgecnc=[1366,3337,2709,9769,1370,3340,2712,9772], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2284),elname="xbrick",eltype="xbrick",typekey=2284) 

        call prepare(lib_xbrick(2285),key=2285, & 
& nodecnc=[2176,2150,2082,2408,4748,4722,4654,4980,24876, 24875,24698, 24697,12136, 12135,24937, 24938 & 
& ,24880, 24879,24702, 24701,12140, 12139,24939, 24940], & 
& edgecnc=[9866,9777,3496,9897,9868,9779,3498,9898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2285),elname="xbrick",eltype="xbrick",typekey=2285) 

        call prepare(lib_xbrick(2286),key=2286, & 
& nodecnc=[194,2151,112,2161,2766,4723,2684,4733,24610, 24609,24941, 24942,24722, 24721,24918, 24917,24614 & 
& , 24613,24943, 24944,24728, 24727,24922, 24921], & 
& edgecnc=[9733,9899,9789,9887,9735,9900,9792,9889], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2286),elname="xbrick",eltype="xbrick",typekey=2286) 

        call prepare(lib_xbrick(2287),key=2287, & 
& nodecnc=[170,2199,2180,2452,2742,4771,4752,5024,11770, 11769,24945, 24946,24932, 24931,24947, 24948 & 
& ,11778, 11777,24949, 24950,24936, 24935,24951, 24952], & 
& edgecnc=[3313,9901,9894,9902,3317,9903,9896,9904], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2287),elname="xbrick",eltype="xbrick",typekey=2287) 

        call prepare(lib_xbrick(2288),key=2288, & 
& nodecnc=[2563,2156,1707,1630,5135,4728,4279,4202,24844, 24843,24320, 24319,22716, 22715,24953, 24954 & 
& ,24848, 24847,24324, 24323,22720, 22719,24955, 24956], & 
& edgecnc=[9850,9588,8786,9905,9852,9590,8788,9906], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2288),elname="xbrick",eltype="xbrick",typekey=2288) 

        call prepare(lib_xbrick(2289),key=2289, & 
& nodecnc=[2178,2186,2159,185,4750,4758,4731,2757,24957, 24958,11898, 11897,24882, 24881,10574, 10573 & 
& ,24959, 24960,11904, 11903,24884, 24883,10582, 10581], & 
& edgecnc=[9907,3377,9869,2715,9908,3380,9870,2719], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2289),elname="xbrick",eltype="xbrick",typekey=2289) 

        call prepare(lib_xbrick(2290),key=2290, & 
& nodecnc=[2160,2177,221,2133,4732,4749,2793,4705,24904, 24903,24961, 24962,24738, 24737,12164, 12163 & 
& ,24908, 24907,24963, 24964,24740, 24739,12172, 12171], & 
& edgecnc=[9880,9909,9797,3510,9882,9910,9798,3514], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2290),elname="xbrick",eltype="xbrick",typekey=2290) 

        call prepare(lib_xbrick(2291),key=2291, & 
& nodecnc=[2178,2162,2187,2191,4750,4734,4759,4763,10580, 10579,11756, 11755,24965, 24966,7886, 7885,10588 & 
& , 10587,11762, 11761,24967, 24968,7894, 7893], & 
& edgecnc=[2718,3306,9911,1371,2722,3309,9912,1375], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2291),elname="xbrick",eltype="xbrick",typekey=2291) 

        call prepare(lib_xbrick(2292),key=2292, & 
& nodecnc=[2325,2185,187,2147,4897,4757,2759,4719,11910, 11909,24969, 24970,11938, 11937,24502, 24501 & 
& ,11916, 11915,24971, 24972,11944, 11943,24508, 24507], & 
& edgecnc=[3383,9913,3397,9679,3386,9914,3400,9682], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2292),elname="xbrick",eltype="xbrick",typekey=2292) 

        call prepare(lib_xbrick(2293),key=2293, & 
& nodecnc=[2514,2164,2174,1930,5086,4736,4746,4502,23532, 23531,24973, 24974,23918, 23917,21268, 21267 & 
& ,23538, 23537,24975, 24976,23922, 23921,21274, 21273], & 
& edgecnc=[9194,9915,9387,8062,9197,9916,9389,8065], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2293),elname="xbrick",eltype="xbrick",typekey=2293) 

        call prepare(lib_xbrick(2294),key=2294, & 
& nodecnc=[2174,2164,2563,1630,4746,4736,5135,4202,24974, 24973,23926, 23925,24954, 24953,21054, 21053 & 
& ,24976, 24975,23932, 23931,24956, 24955,21060, 21059], & 
& edgecnc=[9915,9391,9905,7955,9916,9394,9906,7958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2294),elname="xbrick",eltype="xbrick",typekey=2294) 

        call prepare(lib_xbrick(2295),key=2295, & 
& nodecnc=[2151,2144,2184,112,4723,4716,4756,2684,24522, 24521,24977, 24978,24979, 24980,24942, 24941 & 
& ,24526, 24525,24981, 24982,24983, 24984,24944, 24943], & 
& edgecnc=[9689,9917,9918,9899,9691,9919,9920,9900], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2295),elname="xbrick",eltype="xbrick",typekey=2295) 

        call prepare(lib_xbrick(2296),key=2296, & 
& nodecnc=[170,2452,2165,2198,2742,5024,4737,4770,24948, 24947,24985, 24986,24987, 24988,24989, 24990 & 
& ,24952, 24951,24991, 24992,24993, 24994,24995, 24996], & 
& edgecnc=[9902,9921,9922,9923,9904,9924,9925,9926], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2296),elname="xbrick",eltype="xbrick",typekey=2296) 

        call prepare(lib_xbrick(2297),key=2297, & 
& nodecnc=[2452,2152,2146,2165,5024,4724,4718,4737,24930, 24929,6932, 6931,7054, 7053,24986, 24985,24934 & 
& , 24933,6940, 6939,7062, 7061,24992, 24991], & 
& edgecnc=[9893,894,955,9921,9895,898,959,9924], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2297),elname="xbrick",eltype="xbrick",typekey=2297) 

        call prepare(lib_xbrick(2298),key=2298, & 
& nodecnc=[2168,2180,2199,2190,4740,4752,4771,4762,10504, 10503,24946, 24945,24997, 24998,7872, 7871,10512 & 
& , 10511,24950, 24949,24999, 25000,7880, 7879], & 
& edgecnc=[2680,9901,9927,1364,2684,9903,9928,1368], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2298),elname="xbrick",eltype="xbrick",typekey=2298) 

        call prepare(lib_xbrick(2299),key=2299, & 
& nodecnc=[2190,169,2181,2172,4762,2741,4753,4744,25001, 25002,25003, 25004,11820, 11819,7874, 7873,25005 & 
& , 25006,25007, 25008,11826, 11825,7882, 7881], & 
& edgecnc=[9929,9930,3338,1365,9931,9932,3341,1369], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2299),elname="xbrick",eltype="xbrick",typekey=2299) 

        call prepare(lib_xbrick(2300),key=2300, & 
& nodecnc=[2135,2541,2542,2564,4707,5113,5114,5136,7994, 7993,25009, 25010,25011, 25012,24782, 24781,8002 & 
& , 8001,25013, 25014,25015, 25016,24788, 24787], & 
& edgecnc=[1425,9933,9934,9819,1429,9935,9936,9822], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2300),elname="xbrick",eltype="xbrick",typekey=2300) 

        call prepare(lib_xbrick(2301),key=2301, & 
& nodecnc=[2170,2143,2179,2542,4742,4715,4751,5114,11808, 11807,24886, 24885,25017, 25018,25019, 25020 & 
& ,11814, 11813,24890, 24889,25021, 25022,25023, 25024], & 
& edgecnc=[3332,9871,9937,9938,3335,9873,9939,9940], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2301),elname="xbrick",eltype="xbrick",typekey=2301) 

        call prepare(lib_xbrick(2302),key=2302, & 
& nodecnc=[2196,2182,2323,2324,4768,4754,4895,4896,7088, 7087,25025, 25026,25027, 25028,25029, 25030,7096 & 
& , 7095,25031, 25032,25033, 25034,25035, 25036], & 
& edgecnc=[972,9941,9942,9943,976,9944,9945,9946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2302),elname="xbrick",eltype="xbrick",typekey=2302) 

        call prepare(lib_xbrick(2303),key=2303, & 
& nodecnc=[2171,2186,2178,186,4743,4758,4750,2758,11900, 11899,24958, 24957,7892, 7891,25037, 25038,11906 & 
& , 11905,24960, 24959,7900, 7899,25039, 25040], & 
& edgecnc=[3378,9907,1374,9947,3381,9908,1378,9948], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2303),elname="xbrick",eltype="xbrick",typekey=2303) 

        call prepare(lib_xbrick(2304),key=2304, & 
& nodecnc=[221,2177,2175,2192,2793,4749,4747,4764,24962, 24961,24902, 24901,25041, 25042,11914, 11913 & 
& ,24964, 24963,24906, 24905,25043, 25044,11920, 11919], & 
& edgecnc=[9909,9879,9949,3385,9910,9881,9950,3388], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2304),elname="xbrick",eltype="xbrick",typekey=2304) 

        call prepare(lib_xbrick(2305),key=2305, & 
& nodecnc=[2408,219,2155,2176,4980,2791,4727,4748,12134, 12133,24466, 24465,24830, 24829,24938, 24937 & 
& ,12138, 12137,24470, 24469,24836, 24835,24940, 24939], & 
& edgecnc=[3495,9661,9843,9897,3497,9663,9846,9898], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2305),elname="xbrick",eltype="xbrick",typekey=2305) 

        call prepare(lib_xbrick(2306),key=2306, & 
& nodecnc=[2197,2167,188,2163,4769,4739,2760,4735,25045, 25046,11786, 11785,24926, 24925,7068, 7067,25047 & 
& , 25048,11794, 11793,24928, 24927,7076, 7075], & 
& edgecnc=[9951,3321,9891,962,9952,3325,9892,966], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2306),elname="xbrick",eltype="xbrick",typekey=2306) 

        call prepare(lib_xbrick(2307),key=2307, & 
& nodecnc=[2181,2187,168,2169,4753,4759,2740,4741,25049, 25050,11754, 11753,24896, 24895,11822, 11821 & 
& ,25051, 25052,11760, 11759,24900, 24899,11828, 11827], & 
& edgecnc=[9953,3305,9876,3339,9954,3308,9878,3342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2307),elname="xbrick",eltype="xbrick",typekey=2307) 

        call prepare(lib_xbrick(2308),key=2308, & 
& nodecnc=[2191,2187,2181,169,4763,4759,4753,2741,24966, 24965,25050, 25049,25004, 25003,25053, 25054 & 
& ,24968, 24967,25052, 25051,25008, 25007,25055, 25056], & 
& edgecnc=[9911,9953,9930,9955,9912,9954,9932,9956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2308),elname="xbrick",eltype="xbrick",typekey=2308) 

        call prepare(lib_xbrick(2309),key=2309, & 
& nodecnc=[186,2196,2324,2171,2758,4768,4896,4743,25057, 25058,25030, 25029,7908, 7907,25038, 25037,25059 & 
& , 25060,25036, 25035,7916, 7915,25040, 25039], & 
& edgecnc=[9957,9943,1382,9947,9958,9946,1386,9948], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2309),elname="xbrick",eltype="xbrick",typekey=2309) 

        call prepare(lib_xbrick(2310),key=2310, & 
& nodecnc=[2439,2167,2197,2198,5011,4739,4769,4770,11788, 11787,25046, 25045,25061, 25062,25063, 25064 & 
& ,11796, 11795,25048, 25047,25065, 25066,25067, 25068], & 
& edgecnc=[3322,9951,9959,9960,3326,9952,9961,9962], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2310),elname="xbrick",eltype="xbrick",typekey=2310) 

        call prepare(lib_xbrick(2311),key=2311, & 
& nodecnc=[2195,2203,112,2184,4767,4775,2684,4756,10152, 10151,24718, 24717,24980, 24979,24658, 24657 & 
& ,10160, 10159,24724, 24723,24984, 24983,24664, 24663], & 
& edgecnc=[2504,9787,9918,9757,2508,9790,9920,9760], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2311),elname="xbrick",eltype="xbrick",typekey=2311) 

        call prepare(lib_xbrick(2312),key=2312, & 
& nodecnc=[111,2184,2144,2498,2683,4756,4716,5070,24654, 24653,24978, 24977,24354, 24353,24758, 24757 & 
& ,24660, 24659,24982, 24981,24360, 24359,24760, 24759], & 
& edgecnc=[9755,9917,9605,9807,9758,9919,9608,9808], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2312),elname="xbrick",eltype="xbrick",typekey=2312) 

        call prepare(lib_xbrick(2313),key=2313, & 
& nodecnc=[2200,187,2185,2196,4772,2759,4757,4768,25069, 25070,24970, 24969,7082, 7081,25071, 25072,25073 & 
& , 25074,24972, 24971,7090, 7089,25075, 25076], & 
& edgecnc=[9963,9913,969,9964,9965,9914,973,9966], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2313),elname="xbrick",eltype="xbrick",typekey=2313) 

        call prepare(lib_xbrick(2314),key=2314, & 
& nodecnc=[2068,2195,2188,106,4640,4767,4760,2678,10154, 10153,24792, 24791,24542, 24541,24550, 24549 & 
& ,10162, 10161,24796, 24795,24546, 24545,24552, 24551], & 
& edgecnc=[2505,9824,9699,9703,2509,9826,9701,9704], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2314),elname="xbrick",eltype="xbrick",typekey=2314) 

        call prepare(lib_xbrick(2315),key=2315, & 
& nodecnc=[2472,2112,2188,107,5044,4684,4760,2679,24770, 24769,24544, 24543,24790, 24789,24644, 24643 & 
& ,24774, 24773,24548, 24547,24794, 24793,24650, 24649], & 
& edgecnc=[9813,9700,9823,9750,9815,9702,9825,9753], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2315),elname="xbrick",eltype="xbrick",typekey=2315) 

        call prepare(lib_xbrick(2316),key=2316, & 
& nodecnc=[2124,2396,2189,2142,4696,4968,4761,4714,24598, 24597,24920, 24919,24730, 24729,24710, 24709 & 
& ,24604, 24603,24924, 24923,24734, 24733,24716, 24715], & 
& edgecnc=[9727,9888,9793,9783,9730,9890,9795,9786], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2316),elname="xbrick",eltype="xbrick",typekey=2316) 

        call prepare(lib_xbrick(2317),key=2317, & 
& nodecnc=[2199,2194,169,2190,4771,4766,2741,4762,11768, 11767,25077, 25078,25002, 25001,24998, 24997 & 
& ,11776, 11775,25079, 25080,25006, 25005,25000, 24999], & 
& edgecnc=[3312,9967,9929,9927,3316,9968,9931,9928], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2317),elname="xbrick",eltype="xbrick",typekey=2317) 

        call prepare(lib_xbrick(2318),key=2318, & 
& nodecnc=[2175,2323,2182,2192,4747,4895,4754,4764,24862, 24861,25026, 25025,7086, 7085,25042, 25041,24868 & 
& , 24867,25032, 25031,7094, 7093,25044, 25043], & 
& edgecnc=[9859,9941,971,9949,9862,9944,975,9950], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2318),elname="xbrick",eltype="xbrick",typekey=2318) 

        call prepare(lib_xbrick(2319),key=2319, & 
& nodecnc=[1991,2056,2193,2203,4563,4628,4765,4775,5818, 5817,24620, 24619,24720, 24719,10150, 10149,5826 & 
& , 5825,24626, 24625,24726, 24725,10158, 10157], & 
& edgecnc=[337,9738,9788,2503,341,9741,9791,2507], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2319),elname="xbrick",eltype="xbrick",typekey=2319) 

        call prepare(lib_xbrick(2320),key=2320, & 
& nodecnc=[169,2194,2201,2191,2741,4766,4773,4763,25078, 25077,25081, 25082,7888, 7887,25054, 25053,25080 & 
& , 25079,25083, 25084,7896, 7895,25056, 25055], & 
& edgecnc=[9967,9969,1372,9955,9968,9970,1376,9956], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2320),elname="xbrick",eltype="xbrick",typekey=2320) 

        call prepare(lib_xbrick(2321),key=2321, & 
& nodecnc=[2198,2202,2204,170,4770,4774,4776,2742,25085, 25086,25087, 25088,11772, 11771,24990, 24989 & 
& ,25089, 25090,25091, 25092,11780, 11779,24996, 24995], & 
& edgecnc=[9971,9972,3314,9923,9973,9974,3318,9926], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2321),elname="xbrick",eltype="xbrick",typekey=2321) 

        call prepare(lib_xbrick(2322),key=2322, & 
& nodecnc=[2453,2202,2198,2197,5025,4774,4770,4769,25093, 25094,25086, 25085,25062, 25061,7066, 7065,25095 & 
& , 25096,25090, 25089,25066, 25065,7074, 7073], & 
& edgecnc=[9975,9971,9959,961,9976,9973,9961,965], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2322),elname="xbrick",eltype="xbrick",typekey=2322) 

        call prepare(lib_xbrick(2323),key=2323, & 
& nodecnc=[186,2201,2200,2196,2758,4773,4772,4768,7890, 7889,25097, 25098,25072, 25071,25058, 25057,7898 & 
& , 7897,25099, 25100,25076, 25075,25060, 25059], & 
& edgecnc=[1373,9977,9964,9957,1377,9978,9966,9958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2323),elname="xbrick",eltype="xbrick",typekey=2323) 

        call prepare(lib_xbrick(2324),key=2324, & 
& nodecnc=[2437,187,2200,2453,5009,2759,4772,5025,11934, 11933,25070, 25069,25101, 25102,7072, 7071,11940 & 
& , 11939,25074, 25073,25103, 25104,7080, 7079], & 
& edgecnc=[3395,9963,9979,964,3398,9965,9980,968], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2324),elname="xbrick",eltype="xbrick",typekey=2324) 

        call prepare(lib_xbrick(2325),key=2325, & 
& nodecnc=[2194,2204,2200,2201,4766,4776,4772,4773,11766, 11765,25105, 25106,25098, 25097,25082, 25081 & 
& ,11774, 11773,25107, 25108,25100, 25099,25084, 25083], & 
& edgecnc=[3311,9981,9977,9969,3315,9982,9978,9970], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2325),elname="xbrick",eltype="xbrick",typekey=2325) 

        call prepare(lib_xbrick(2326),key=2326, & 
& nodecnc=[2204,2202,2453,2200,4776,4774,5025,4772,25088, 25087,25094, 25093,25102, 25101,25106, 25105 & 
& ,25092, 25091,25096, 25095,25104, 25103,25108, 25107], & 
& edgecnc=[9972,9975,9979,9981,9974,9976,9980,9982], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2326),elname="xbrick",eltype="xbrick",typekey=2326) 

        call prepare(lib_xbrick(2327),key=2327, & 
& nodecnc=[2213,1370,2211,2212,4785,3942,4783,4784,21180, 21179,11030, 11029,21298, 21297,25109, 25110 & 
& ,21184, 21183,11038, 11037,21306, 21305,25111, 25112], & 
& edgecnc=[8018,2943,8077,9983,8020,2947,8081,9984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2327),elname="xbrick",eltype="xbrick",typekey=2327) 

        call prepare(lib_xbrick(2328),key=2328, & 
& nodecnc=[2213,2212,1382,237,4785,4784,3954,2809,25110, 25109,21296, 21295,8422, 8421,21082, 21081,25112 & 
& , 25111,21304, 21303,8430, 8429,21088, 21087], & 
& edgecnc=[9983,8076,1639,7969,9984,8080,1643,7972], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2328),elname="xbrick",eltype="xbrick",typekey=2328) 

        call prepare(lib_xbrick(2329),key=2329, & 
& nodecnc=[585,553,519,2216,3157,3125,3091,4788,14682, 14681,14318, 14317,14326, 14325,14490, 14489,14688 & 
& , 14687,14320, 14319,14332, 14331,14496, 14495], & 
& edgecnc=[4769,4587,4591,4673,4772,4588,4594,4676], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2329),elname="xbrick",eltype="xbrick",typekey=2329) 

        call prepare(lib_xbrick(2330),key=2330, & 
& nodecnc=[2228,2227,1718,230,4800,4799,4290,2802,22968, 22967,6808, 6807,21942, 21941,7244, 7243,22972 & 
& , 22971,6816, 6815,21944, 21943,7252, 7251], & 
& edgecnc=[8912,832,8399,1050,8914,836,8400,1054], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2330),elname="xbrick",eltype="xbrick",typekey=2330) 

        call prepare(lib_xbrick(2331),key=2331, & 
& nodecnc=[1140,343,1287,2233,3712,2915,3859,4805,19548, 19547,20590, 20589,11266, 11265,19306, 19305 & 
& ,19554, 19553,20592, 20591,11274, 11273,19312, 19311], & 
& edgecnc=[7202,7723,3061,7081,7205,7724,3065,7084], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2331),elname="xbrick",eltype="xbrick",typekey=2331) 

        call prepare(lib_xbrick(2332),key=2332, & 
& nodecnc=[1125,2251,2235,1618,3697,4823,4807,4190,19356, 19355,19624, 19623,20166, 20165,19178, 19177 & 
& ,19360, 19359,19628, 19627,20168, 20167,19180, 19179], & 
& edgecnc=[7106,7240,7511,7017,7108,7242,7512,7018], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2332),elname="xbrick",eltype="xbrick",typekey=2332) 

        call prepare(lib_xbrick(2333),key=2333, & 
& nodecnc=[1118,2296,1145,2236,3690,4868,3717,4808,25113, 25114,19260, 19259,19752, 19751,19294, 19293 & 
& ,25115, 25116,19266, 19265,19756, 19755,19300, 19299], & 
& edgecnc=[9985,7058,7304,7075,9986,7061,7306,7078], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2333),elname="xbrick",eltype="xbrick",typekey=2333) 

        call prepare(lib_xbrick(2334),key=2334, & 
& nodecnc=[2238,2237,2058,262,4810,4809,4630,2834,7098, 7097,7130, 7129,24198, 24197,24692, 24691,7106 & 
& , 7105,7136, 7135,24202, 24201,24696, 24695], & 
& edgecnc=[977,993,9527,9774,981,996,9529,9776], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2334),elname="xbrick",eltype="xbrick",typekey=2334) 

        call prepare(lib_xbrick(2335),key=2335, & 
& nodecnc=[635,2268,602,2240,3207,4840,3174,4812,15426, 15425,15090, 15089,15074, 15073,15066, 15065,15432 & 
& , 15431,15094, 15093,15076, 15075,15072, 15071], & 
& edgecnc=[5141,4973,4965,4961,5144,4975,4966,4964], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2335),elname="xbrick",eltype="xbrick",typekey=2335) 

        call prepare(lib_xbrick(2336),key=2336, & 
& nodecnc=[2050,1926,2243,2244,4622,4498,4815,4816,23902, 23901,10196, 10195,23938, 23937,24232, 24231 & 
& ,23906, 23905,10200, 10199,23942, 23941,24236, 24235], & 
& edgecnc=[9379,2526,9397,9544,9381,2528,9399,9546], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2336),elname="xbrick",eltype="xbrick",typekey=2336) 

        call prepare(lib_xbrick(2337),key=2337, & 
& nodecnc=[1346,270,1650,2255,3918,2842,4222,4827,20974, 20973,21468, 21467,22510, 22509,21522, 21521 & 
& ,20976, 20975,21472, 21471,22512, 22511,21524, 21523], & 
& edgecnc=[7915,8162,8683,8189,7916,8164,8684,8190], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2337),elname="xbrick",eltype="xbrick",typekey=2337) 

        call prepare(lib_xbrick(2338),key=2338, & 
& nodecnc=[1063,2285,1107,2258,3635,4857,3679,4830,19142, 19141,19128, 19127,18944, 18943,19482, 19481 & 
& ,19146, 19145,19136, 19135,18952, 18951,19486, 19485], & 
& edgecnc=[6999,6992,6900,7169,7001,6996,6904,7171], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2338),elname="xbrick",eltype="xbrick",typekey=2338) 

        call prepare(lib_xbrick(2339),key=2339, & 
& nodecnc=[2247,626,672,2266,4819,3198,3244,4838,14890, 14889,14850, 14849,15770, 15769,15736, 15735,14892 & 
& , 14891,14856, 14855,15774, 15773,15740, 15739], & 
& edgecnc=[4873,4853,5313,5296,4874,4856,5315,5298], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2339),elname="xbrick",eltype="xbrick",typekey=2339) 

        call prepare(lib_xbrick(2340),key=2340, & 
& nodecnc=[655,2347,2269,622,3227,4919,4841,3194,15260, 15259,15716, 15715,15454, 15453,15570, 15569,15268 & 
& , 15267,15722, 15721,15456, 15455,15572, 15571], & 
& edgecnc=[5058,5286,5155,5213,5062,5289,5156,5214], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2340),elname="xbrick",eltype="xbrick",typekey=2340) 

        call prepare(lib_xbrick(2341),key=2341, & 
& nodecnc=[2282,2327,877,2281,4854,4899,3449,4853,18620, 18619,17452, 17451,17746, 17745,25117, 25118 & 
& ,18626, 18625,17456, 17455,17750, 17749,25119, 25120], & 
& edgecnc=[6738,6154,6301,9987,6741,6156,6303,9988], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2341),elname="xbrick",eltype="xbrick",typekey=2341) 

        call prepare(lib_xbrick(2342),key=2342, & 
& nodecnc=[989,2282,2281,997,3561,4854,4853,3569,18622, 18621,25118, 25117,17916, 17915,6670, 6669,18628 & 
& , 18627,25120, 25119,17920, 17919,6678, 6677], & 
& edgecnc=[6739,9987,6386,763,6742,9988,6388,767], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2342),elname="xbrick",eltype="xbrick",typekey=2342) 

        call prepare(lib_xbrick(2343),key=2343, & 
& nodecnc=[2292,843,2353,2327,4864,3415,4925,4899,17770, 17769,17174, 17173,17450, 17449,18618, 18617 & 
& ,17772, 17771,17176, 17175,17454, 17453,18624, 18623], & 
& edgecnc=[6313,6015,6153,6737,6314,6016,6155,6740], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2343),elname="xbrick",eltype="xbrick",typekey=2343) 

        call prepare(lib_xbrick(2344),key=2344, & 
& nodecnc=[2326,1967,2284,2016,4898,4539,4856,4588,24390, 24389,12462, 12461,12142, 12141,24382, 24381 & 
& ,24394, 24393,12470, 12469,12150, 12149,24386, 24385], & 
& edgecnc=[9623,3659,3499,9619,9625,3663,3503,9621], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2344),elname="xbrick",eltype="xbrick",typekey=2344) 

        call prepare(lib_xbrick(2345),key=2345, & 
& nodecnc=[1456,2288,316,2303,4028,4860,2888,4875,22336, 22335,12834, 12833,25121, 25122,21826, 21825 & 
& ,22340, 22339,12842, 12841,25123, 25124,21828, 21827], & 
& edgecnc=[8596,3845,9989,8341,8598,3849,9990,8342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2345),elname="xbrick",eltype="xbrick",typekey=2345) 

        call prepare(lib_xbrick(2346),key=2346, & 
& nodecnc=[1084,1062,1140,2290,3656,3634,3712,4862,18708, 18707,19550, 19549,19310, 19309,19106, 19105 & 
& ,18712, 18711,19556, 19555,19316, 19315,19112, 19111], & 
& edgecnc=[6782,7203,7083,6981,6784,7206,7086,6984], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2346),elname="xbrick",eltype="xbrick",typekey=2346) 

        call prepare(lib_xbrick(2347),key=2347, & 
& nodecnc=[2291,268,1394,1402,4863,2840,3966,3974,12248, 12247,21418, 21417,21546, 21545,21566, 21565 & 
& ,12256, 12255,21420, 21419,21548, 21547,21568, 21567], & 
& edgecnc=[3552,8137,8201,8211,3556,8138,8202,8212], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2347),elname="xbrick",eltype="xbrick",typekey=2347) 

        call prepare(lib_xbrick(2348),key=2348, & 
& nodecnc=[669,2320,632,2293,3241,4892,3204,4865,15650, 15649,15382, 15381,15390, 15389,15438, 15437,15656 & 
& , 15655,15386, 15385,15392, 15391,15442, 15441], & 
& edgecnc=[5253,5119,5123,5147,5256,5121,5124,5149], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2348),elname="xbrick",eltype="xbrick",typekey=2348) 

        call prepare(lib_xbrick(2349),key=2349, & 
& nodecnc=[2290,2296,1118,2294,4862,4868,3690,4866,19308, 19307,25114, 25113,19542, 19541,19108, 19107 & 
& ,19314, 19313,25116, 25115,19544, 19543,19114, 19113], & 
& edgecnc=[7082,9985,7199,6982,7085,9986,7200,6985], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2349),elname="xbrick",eltype="xbrick",typekey=2349) 

        call prepare(lib_xbrick(2350),key=2350, & 
& nodecnc=[1826,2299,1894,1739,4398,4871,4466,4311,23010, 23009,23694, 23693,22816, 22815,22826, 22825 & 
& ,23016, 23015,23700, 23699,22822, 22821,22828, 22827], & 
& edgecnc=[8933,9275,8836,8841,8936,9278,8839,8842], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2350),elname="xbrick",eltype="xbrick",typekey=2350) 

        call prepare(lib_xbrick(2351),key=2351, & 
& nodecnc=[2301,2300,1908,223,4873,4872,4480,2795,23702, 23701,23044, 23043,24182, 24181,25125, 25126 & 
& ,23708, 23707,23048, 23047,24184, 24183,25127, 25128], & 
& edgecnc=[9279,8950,9519,9991,9282,8952,9520,9992], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2351),elname="xbrick",eltype="xbrick",typekey=2351) 

        call prepare(lib_xbrick(2352),key=2352, & 
& nodecnc=[2307,2306,760,788,4879,4878,3332,3360,16736, 16735,16978, 16977,16006, 16005,16974, 16973,16740 & 
& , 16739,16980, 16979,16012, 16011,16976, 16975], & 
& edgecnc=[5796,5917,5431,5915,5798,5918,5434,5916], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2352),elname="xbrick",eltype="xbrick",typekey=2352) 

        call prepare(lib_xbrick(2353),key=2353, & 
& nodecnc=[2309,2308,2264,606,4881,4880,4836,3178,15794, 15793,15530, 15529,14410, 14409,6572, 6571,15802 & 
& , 15801,15534, 15533,14418, 14417,6580, 6579], & 
& edgecnc=[5325,5193,4633,714,5329,5195,4637,718], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2353),elname="xbrick",eltype="xbrick",typekey=2353) 

        call prepare(lib_xbrick(2354),key=2354, & 
& nodecnc=[2313,2312,635,671,4885,4884,3207,3243,25129, 25130,15428, 15427,15064, 15063,15706, 15705,25131 & 
& , 25132,15434, 15433,15070, 15069,15710, 15709], & 
& edgecnc=[9993,5142,4960,5281,9994,5145,4963,5283], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2354),elname="xbrick",eltype="xbrick",typekey=2354) 

        call prepare(lib_xbrick(2355),key=2355, & 
& nodecnc=[741,2370,2312,2313,3313,4942,4884,4885,16078, 16077,16090, 16089,25130, 25129,6628, 6627,16086 & 
& , 16085,16092, 16091,25132, 25131,6636, 6635], & 
& edgecnc=[5467,5473,9993,742,5471,5474,9994,746], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2355),elname="xbrick",eltype="xbrick",typekey=2355) 

        call prepare(lib_xbrick(2356),key=2356, & 
& nodecnc=[713,2343,664,2315,3285,4915,3236,4887,25133, 25134,15660, 15659,15370, 15369,15140, 15139,25135 & 
& , 25136,15666, 15665,15374, 15373,15148, 15147], & 
& edgecnc=[9995,5258,5113,4998,9996,5261,5115,5002], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2356),elname="xbrick",eltype="xbrick",typekey=2356) 

        call prepare(lib_xbrick(2357),key=2357, & 
& nodecnc=[725,2346,678,2316,3297,4918,3250,4888,16328, 16327,15136, 15135,15554, 15553,16200, 16199,16334 & 
& , 16333,15144, 15143,15556, 15555,16204, 16203], & 
& edgecnc=[5592,4996,5205,5528,5595,5000,5206,5530], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2357),elname="xbrick",eltype="xbrick",typekey=2357) 

        call prepare(lib_xbrick(2358),key=2358, & 
& nodecnc=[2319,2318,750,791,4891,4890,3322,3363,17410, 17409,16990, 16989,15926, 15925,16692, 16691,17412 & 
& , 17411,16992, 16991,15930, 15929,16698, 16697], & 
& edgecnc=[6133,5923,5391,5774,6134,5924,5393,5777], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2358),elname="xbrick",eltype="xbrick",typekey=2358) 

        call prepare(lib_xbrick(2359),key=2359, & 
& nodecnc=[701,2389,662,2320,3273,4961,3234,4892,25137, 25138,25139, 25140,15384, 15383,15648, 15647,25141 & 
& , 25142,25143, 25144,15388, 15387,15654, 15653], & 
& edgecnc=[9997,9998,5120,5252,9999,10000,5122,5255], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2359),elname="xbrick",eltype="xbrick",typekey=2359) 

        call prepare(lib_xbrick(2360),key=2360, & 
& nodecnc=[211,2390,1308,2322,2783,4962,3880,4894,19430, 19429,12094, 12093,20792, 20791,20710, 20709 & 
& ,19438, 19437,12102, 12101,20798, 20797,20714, 20713], & 
& edgecnc=[7143,3475,7824,7783,7147,3479,7827,7785], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2360),elname="xbrick",eltype="xbrick",typekey=2360) 

        call prepare(lib_xbrick(2361),key=2361, & 
& nodecnc=[2324,2323,2173,220,4896,4895,4745,2792,25028, 25027,24866, 24865,24910, 24909,7902, 7901,25034 & 
& , 25033,24872, 24871,24912, 24911,7910, 7909], & 
& edgecnc=[9942,9861,9883,1379,9945,9864,9884,1383], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2361),elname="xbrick",eltype="xbrick",typekey=2361) 

        call prepare(lib_xbrick(2362),key=2362, & 
& nodecnc=[823,2363,754,2338,3395,4935,3326,4910,17200, 17199,16950, 16949,16230, 16229,16940, 16939,17206 & 
& , 17205,16952, 16951,16234, 16233,16946, 16945], & 
& edgecnc=[6028,5903,5543,5898,6031,5904,5545,5901], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2362),elname="xbrick",eltype="xbrick",typekey=2362) 

        call prepare(lib_xbrick(2363),key=2363, & 
& nodecnc=[657,2388,707,2339,3229,4960,3279,4911,15606, 15605,16124, 16123,8698, 8697,15614, 15613,15610 & 
& , 15609,16128, 16127,8706, 8705,15616, 15615], & 
& edgecnc=[5231,5490,1777,5235,5233,5492,1781,5236], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2363),elname="xbrick",eltype="xbrick",typekey=2363) 

        call prepare(lib_xbrick(2364),key=2364, & 
& nodecnc=[2341,2375,747,719,4913,4947,3319,3291,16646, 16645,16630, 16629,15874, 15873,15560, 15559,16654 & 
& , 16653,16636, 16635,15878, 15877,15566, 15565], & 
& edgecnc=[5751,5743,5365,5208,5755,5746,5367,5211], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2364),elname="xbrick",eltype="xbrick",typekey=2364) 

        call prepare(lib_xbrick(2365),key=2365, & 
& nodecnc=[763,2343,713,2398,3335,4915,3285,4970,16314, 16313,25134, 25133,16704, 16703,17212, 17211,16320 & 
& , 16319,25136, 25135,16708, 16707,17216, 17215], & 
& edgecnc=[5585,9995,5780,6034,5588,9996,5782,6036], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2365),elname="xbrick",eltype="xbrick",typekey=2365) 

        call prepare(lib_xbrick(2366),key=2366, & 
& nodecnc=[2344,2345,154,2017,4916,4917,2726,4589,24398, 24397,24818, 24817,10166, 10165,24176, 24175 & 
& ,24400, 24399,24820, 24819,10172, 10171,24180, 24179], & 
& edgecnc=[9627,9837,2511,9516,9628,9838,2514,9518], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2366),elname="xbrick",eltype="xbrick",typekey=2366) 

        call prepare(lib_xbrick(2367),key=2367, & 
& nodecnc=[2298,2348,2303,316,4870,4920,4875,2888,9516, 9515,21554, 21553,25122, 25121,21598, 21597,9524 & 
& , 9523,21560, 21559,25124, 25123,21600, 21599], & 
& edgecnc=[2186,8205,9989,8227,2190,8208,9990,8228], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2367),elname="xbrick",eltype="xbrick",typekey=2367) 

        call prepare(lib_xbrick(2368),key=2368, & 
& nodecnc=[739,2351,2350,700,3311,4923,4922,3272,25145, 25146,16800, 16799,6686, 6685,15698, 15697,25147 & 
& , 25148,16806, 16805,6694, 6693,15702, 15701], & 
& edgecnc=[10001,5828,771,5277,10002,5831,775,5279], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2368),elname="xbrick",eltype="xbrick",typekey=2368) 

        call prepare(lib_xbrick(2369),key=2369, & 
& nodecnc=[2369,2352,659,698,4941,4924,3231,3270,16288, 16287,16014, 16013,15622, 15621,16018, 16017,16294 & 
& , 16293,16016, 16015,15628, 15627,16026, 16025], & 
& edgecnc=[5572,5435,5239,5437,5575,5436,5242,5441], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2369),elname="xbrick",eltype="xbrick",typekey=2369) 

        call prepare(lib_xbrick(2370),key=2370, & 
& nodecnc=[2090,2301,223,2354,4662,4873,2795,4926,23704, 23703,25126, 25125,7442, 7441,11924, 11923,23710 & 
& , 23709,25128, 25127,7450, 7449,11930, 11929], & 
& edgecnc=[9280,9991,1149,3390,9283,9992,1153,3393], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2370),elname="xbrick",eltype="xbrick",typekey=2370) 

        call prepare(lib_xbrick(2371),key=2371, & 
& nodecnc=[286,1823,1742,2356,2858,4395,4314,4928,5782, 5781,22668, 22667,5970, 5969,23408, 23407,5790 & 
& , 5789,22672, 22671,5978, 5977,23412, 23411], & 
& edgecnc=[319,8762,413,9132,323,8764,417,9134], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2371),elname="xbrick",eltype="xbrick",typekey=2371) 

        call prepare(lib_xbrick(2372),key=2372, & 
& nodecnc=[2358,2357,1910,289,4930,4929,4482,2861,23726, 23725,23096, 23095,24146, 24145,25149, 25150 & 
& ,23730, 23729,23100, 23099,24150, 24149,25151, 25152], & 
& edgecnc=[9291,8976,9501,10003,9293,8978,9503,10004], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2372),elname="xbrick",eltype="xbrick",typekey=2372) 

        call prepare(lib_xbrick(2373),key=2373, & 
& nodecnc=[2359,1341,1348,2450,4931,3913,3920,5022,21666, 21665,21128, 21127,20954, 20953,21044, 21043 & 
& ,21668, 21667,21132, 21131,20960, 20959,21048, 21047], & 
& edgecnc=[8261,7992,7905,7950,8262,7994,7908,7952], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2373),elname="xbrick",eltype="xbrick",typekey=2373) 

        call prepare(lib_xbrick(2374),key=2374, & 
& nodecnc=[2361,2360,1909,2037,4933,4932,4481,4609,23722, 23721,23080, 23079,24162, 24161,24514, 24513 & 
& ,23724, 23723,23084, 23083,24164, 24163,24516, 24515], & 
& edgecnc=[9289,8968,9509,9685,9290,8970,9510,9686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2374),elname="xbrick",eltype="xbrick",typekey=2374) 

        call prepare(lib_xbrick(2375),key=2375, & 
& nodecnc=[2366,2365,680,730,4938,4937,3252,3302,16304, 16303,15538, 15537,15502, 15501,16622, 16621,16312 & 
& , 16311,15544, 15543,15506, 15505,16626, 16625], & 
& edgecnc=[5580,5197,5179,5739,5584,5200,5181,5741], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2375),elname="xbrick",eltype="xbrick",typekey=2375) 

        call prepare(lib_xbrick(2376),key=2376, & 
& nodecnc=[2364,755,2365,2420,4936,3327,4937,4992,16882, 16881,15540, 15539,16612, 16611,16602, 16601 & 
& ,16884, 16883,15546, 15545,16618, 16617,16606, 16605], & 
& edgecnc=[5869,5198,5734,5729,5870,5201,5737,5731], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2376),elname="xbrick",eltype="xbrick",typekey=2376) 

        call prepare(lib_xbrick(2377),key=2377, & 
& nodecnc=[2401,698,2368,704,4973,3270,4940,3276,16020, 16019,15626, 15625,15728, 15727,16074, 16073,16028 & 
& , 16027,15632, 15631,15732, 15731,16082, 16081], & 
& edgecnc=[5438,5241,5292,5465,5442,5244,5294,5469], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2377),elname="xbrick",eltype="xbrick",typekey=2377) 

        call prepare(lib_xbrick(2378),key=2378, & 
& nodecnc=[775,2401,741,2371,3347,4973,3313,4943,16452, 16451,16080, 16079,6626, 6625,16098, 16097,16456 & 
& , 16455,16088, 16087,6634, 6633,16104, 16103], & 
& edgecnc=[5654,5468,741,5477,5656,5472,745,5480], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2378),elname="xbrick",eltype="xbrick",typekey=2378) 

        call prepare(lib_xbrick(2379),key=2379, & 
& nodecnc=[319,1749,1661,2372,2891,4321,4233,4944,22896, 22895,12888, 12887,22554, 22553,22436, 22435 & 
& ,22900, 22899,12892, 12891,22556, 22555,22440, 22439], & 
& edgecnc=[8876,3872,8705,8646,8878,3874,8706,8648], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2379),elname="xbrick",eltype="xbrick",typekey=2379) 

        call prepare(lib_xbrick(2380),key=2380, & 
& nodecnc=[2424,740,2373,208,4996,3312,4945,2780,16442, 16441,16054, 16053,25153, 25154,8346, 8345,16448 & 
& , 16447,16062, 16061,25155, 25156,8354, 8353], & 
& edgecnc=[5649,5455,10005,1601,5652,5459,10006,1605], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2380),elname="xbrick",eltype="xbrick",typekey=2380) 

        call prepare(lib_xbrick(2381),key=2381, & 
& nodecnc=[834,208,2373,2425,3406,2780,4945,4997,25157, 25158,25154, 25153,16190, 16189,12066, 12065,25159 & 
& , 25160,25156, 25155,16194, 16193,12074, 12073], & 
& edgecnc=[10007,10005,5523,3461,10008,10006,5525,3465], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2381),elname="xbrick",eltype="xbrick",typekey=2381) 

        call prepare(lib_xbrick(2382),key=2382, & 
& nodecnc=[763,2416,753,2374,3335,4988,3325,4946,17210, 17209,16596, 16595,16590, 16589,16316, 16315,17214 & 
& , 17213,16600, 16599,16592, 16591,16322, 16321], & 
& edgecnc=[6033,5726,5723,5586,6035,5728,5724,5589], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2382),elname="xbrick",eltype="xbrick",typekey=2382) 

        call prepare(lib_xbrick(2383),key=2383, & 
& nodecnc=[2321,686,662,2389,4893,3258,3234,4961,15750, 15749,15046, 15045,25140, 25139,16422, 16421,15756 & 
& , 15755,15054, 15053,25144, 25143,16426, 16425], & 
& edgecnc=[5303,4951,9998,5639,5306,4955,10000,5641], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2383),elname="xbrick",eltype="xbrick",typekey=2383) 

        call prepare(lib_xbrick(2384),key=2384, & 
& nodecnc=[285,1838,1760,2380,2857,4410,4332,4952,24306, 24305,12402, 12401,22992, 22991,23446, 23445 & 
& ,24310, 24309,12408, 12407,22996, 22995,23452, 23451], & 
& edgecnc=[9581,3629,8924,9151,9583,3632,8926,9154], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2384),elname="xbrick",eltype="xbrick",typekey=2384) 

        call prepare(lib_xbrick(2385),key=2385, & 
& nodecnc=[2383,2382,1943,1935,4955,4954,4515,4507,24460, 24459,24402, 24401,23806, 23805,23874, 23873 & 
& ,24464, 24463,24406, 24405,23810, 23809,23876, 23875], & 
& edgecnc=[9658,9629,9331,9365,9660,9631,9333,9366], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2385),elname="xbrick",eltype="xbrick",typekey=2385) 

        call prepare(lib_xbrick(2386),key=2386, & 
& nodecnc=[280,2385,2384,1887,2852,4957,4956,4459,23514, 23513,22572, 22571,23642, 23641,22886, 22885 & 
& ,23516, 23515,22580, 22579,23644, 23643,22892, 22891], & 
& edgecnc=[9185,8714,9249,8871,9186,8718,9250,8874], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2386),elname="xbrick",eltype="xbrick",typekey=2386) 

        call prepare(lib_xbrick(2387),key=2387, & 
& nodecnc=[212,1316,2337,2302,2784,3888,4909,4874,20886, 20885,20612, 20611,20896, 20895,21310, 21309 & 
& ,20890, 20889,20618, 20617,20900, 20899,21312, 21311], & 
& edgecnc=[7871,7734,7876,8083,7873,7737,7878,8084], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2387),elname="xbrick",eltype="xbrick",typekey=2387) 

        call prepare(lib_xbrick(2388),key=2388, & 
& nodecnc=[2406,2394,743,759,4978,4966,3315,3331,16464, 16463,16458, 16457,16162, 16161,16224, 16223,16470 & 
& , 16469,16460, 16459,16164, 16163,16228, 16227], & 
& edgecnc=[5660,5657,5509,5540,5663,5658,5510,5542], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2388),elname="xbrick",eltype="xbrick",typekey=2388) 

        call prepare(lib_xbrick(2389),key=2389, & 
& nodecnc=[818,2410,804,2375,3390,4982,3376,4947,17250, 17249,17524, 17523,16632, 16631,16644, 16643,17256 & 
& , 17255,17528, 17527,16638, 16637,16652, 16651], & 
& edgecnc=[6053,6190,5744,5750,6056,6192,5747,5754], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2389),elname="xbrick",eltype="xbrick",typekey=2389) 

        call prepare(lib_xbrick(2390),key=2390, & 
& nodecnc=[909,872,835,2404,3481,3444,3407,4976,18004, 18003,17150, 17149,16668, 16667,6442, 6441,18010 & 
& , 18009,17152, 17151,16676, 16675,6450, 6449], & 
& edgecnc=[6430,6003,5762,649,6433,6004,5766,653], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2390),elname="xbrick",eltype="xbrick",typekey=2390) 

        call prepare(lib_xbrick(2391),key=2391, & 
& nodecnc=[827,2469,781,2399,3399,5041,3353,4971,17344, 17343,16570, 16569,16330, 16329,15858, 15857,17352 & 
& , 17351,16578, 16577,16336, 16335,15866, 15865], & 
& edgecnc=[6100,5713,5593,5357,6104,5717,5596,5361], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2391),elname="xbrick",eltype="xbrick",typekey=2391) 

        call prepare(lib_xbrick(2392),key=2392, & 
& nodecnc=[2407,2402,2341,757,4979,4974,4913,3329,25161, 25162,16648, 16647,15558, 15557,15934, 15933 & 
& ,25163, 25164,16656, 16655,15564, 15563,15940, 15939], & 
& edgecnc=[10009,5752,5207,5395,10010,5756,5210,5398], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2392),elname="xbrick",eltype="xbrick",typekey=2392) 

        call prepare(lib_xbrick(2393),key=2393, & 
& nodecnc=[2092,2358,289,2411,4664,4930,2861,4983,23996, 23995,25150, 25149,24154, 24153,6124, 6123,24002 & 
& , 24001,25152, 25151,24158, 24157,6132, 6131], & 
& edgecnc=[9426,10003,9505,490,9429,10004,9507,494], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2393),elname="xbrick",eltype="xbrick",typekey=2393) 

        call prepare(lib_xbrick(2394),key=2394, & 
& nodecnc=[838,792,2415,826,3410,3364,4987,3398,17178, 17177,16810, 16809,17648, 17647,16754, 16753,17182 & 
& , 17181,16818, 16817,17652, 17651,16762, 16761], & 
& edgecnc=[6017,5833,6252,5805,6019,5837,6254,5809], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2394),elname="xbrick",eltype="xbrick",typekey=2394) 

        call prepare(lib_xbrick(2395),key=2395, & 
& nodecnc=[1999,285,2418,2458,4571,2857,4990,5030,24308, 24307,23444, 23443,24474, 24473,23824, 23823 & 
& ,24312, 24311,23450, 23449,24478, 24477,23828, 23827], & 
& edgecnc=[9582,9150,9665,9340,9584,9153,9667,9342], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2395),elname="xbrick",eltype="xbrick",typekey=2395) 

        call prepare(lib_xbrick(2396),key=2396, & 
& nodecnc=[825,778,2420,2455,3397,3350,4992,5027,17156, 17155,16604, 16603,16610, 16609,16876, 16875,17160 & 
& , 17159,16608, 16607,16616, 16615,16880, 16879], & 
& edgecnc=[6006,5730,5733,5866,6008,5732,5736,5868], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2396),elname="xbrick",eltype="xbrick",typekey=2396) 

        call prepare(lib_xbrick(2397),key=2397, & 
& nodecnc=[2389,701,809,722,4961,3273,3381,3294,25138, 25137,16044, 16043,16272, 16271,16424, 16423,25142 & 
& , 25141,16050, 16049,16280, 16279,16428, 16427], & 
& edgecnc=[9997,5450,5564,5640,9999,5453,5568,5642], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2397),elname="xbrick",eltype="xbrick",typekey=2397) 

        call prepare(lib_xbrick(2398),key=2398, & 
& nodecnc=[2155,2079,2030,2422,4727,4651,4602,4994,24468, 24467,24130, 24129,10618, 10617,25165, 25166 & 
& ,24472, 24471,24134, 24133,10626, 10625,25167, 25168], & 
& edgecnc=[9662,9493,2737,10011,9664,9495,2741,10012], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2398),elname="xbrick",eltype="xbrick",typekey=2398) 

        call prepare(lib_xbrick(2399),key=2399, & 
& nodecnc=[208,834,2429,2461,2780,3406,5001,5033,25158, 25157,17398, 17397,25169, 25170,8348, 8347,25160 & 
& , 25159,17404, 17403,25171, 25172,8356, 8355], & 
& edgecnc=[10007,6127,10013,1602,10008,6130,10014,1606], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2399),elname="xbrick",eltype="xbrick",typekey=2399) 

        call prepare(lib_xbrick(2400),key=2400, & 
& nodecnc=[793,2430,738,2405,3365,5002,3310,4977,16836, 16835,16430, 16429,16450, 16449,16848, 16847,16842 & 
& , 16841,16434, 16433,16454, 16453,16854, 16853], & 
& edgecnc=[5846,5643,5653,5852,5849,5645,5655,5855], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2400),elname="xbrick",eltype="xbrick",typekey=2400) 

        call prepare(lib_xbrick(2401),key=2401, & 
& nodecnc=[844,876,2434,2481,3416,3448,5006,5053,16520, 16519,17636, 17635,25173, 25174,25175, 25176,16528 & 
& , 16527,17642, 17641,25177, 25178,25179, 25180], & 
& edgecnc=[5688,6246,10015,10016,5692,6249,10017,10018], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2401),elname="xbrick",eltype="xbrick",typekey=2401) 

        call prepare(lib_xbrick(2402),key=2402, & 
& nodecnc=[862,824,859,2435,3434,3396,3431,5007,16166, 16165,16734, 16733,25181, 25182,17606, 17605,16174 & 
& , 16173,16738, 16737,25183, 25184,17612, 17611], & 
& edgecnc=[5511,5795,10019,6231,5515,5797,10020,6234], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2402),elname="xbrick",eltype="xbrick",typekey=2402) 

        call prepare(lib_xbrick(2403),key=2403, & 
& nodecnc=[2038,2326,222,2354,4610,4898,2794,4926,24392, 24391,24384, 24383,11926, 11925,7448, 7447,24396 & 
& , 24395,24388, 24387,11932, 11931,7456, 7455], & 
& edgecnc=[9624,9620,3391,1152,9626,9622,3394,1156], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2403),elname="xbrick",eltype="xbrick",typekey=2403) 

        call prepare(lib_xbrick(2404),key=2404, & 
& nodecnc=[2482,985,2438,995,5054,3557,5010,3567,18682, 18681,18276, 18275,8302, 8301,18592, 18591,18688 & 
& , 18687,18282, 18281,8310, 8309,18598, 18597], & 
& edgecnc=[6769,6566,1579,6724,6772,6569,1583,6727], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2404),elname="xbrick",eltype="xbrick",typekey=2404) 

        call prepare(lib_xbrick(2405),key=2405, & 
& nodecnc=[2183,171,2122,2439,4755,2743,4694,5011,7050, 7049,24578, 24577,11782, 11781,25185, 25186,7058 & 
& , 7057,24582, 24581,11790, 11789,25187, 25188], & 
& edgecnc=[953,9717,3319,10021,957,9719,3323,10022], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2405),elname="xbrick",eltype="xbrick",typekey=2405) 

        call prepare(lib_xbrick(2406),key=2406, & 
& nodecnc=[2500,133,1274,2440,5072,2705,3846,5012,20758, 20757,10922, 10921,20604, 20603,20698, 20697 & 
& ,20760, 20759,10928, 10927,20608, 20607,20700, 20699], & 
& edgecnc=[7807,2889,7730,7777,7808,2892,7732,7778], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2406),elname="xbrick",eltype="xbrick",typekey=2406) 

        call prepare(lib_xbrick(2407),key=2407, & 
& nodecnc=[2351,739,774,2441,4923,3311,3346,5013,25146, 25145,16440, 16439,16814, 16813,16828, 16827,25148 & 
& , 25147,16446, 16445,16822, 16821,16832, 16831], & 
& edgecnc=[10001,5648,5835,5842,10002,5651,5839,5844], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2407),elname="xbrick",eltype="xbrick",typekey=2407) 

        call prepare(lib_xbrick(2408),key=2408, & 
& nodecnc=[2443,1005,1016,2442,5015,3577,3588,5014,8456, 8455,18466, 18465,19240, 19239,12050, 12049,8464 & 
& , 8463,18472, 18471,19246, 19245,12058, 12057], & 
& edgecnc=[1656,6661,7048,3453,1660,6664,7051,3457], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2408),elname="xbrick",eltype="xbrick",typekey=2408) 

        call prepare(lib_xbrick(2409),key=2409, & 
& nodecnc=[2491,202,2448,2449,5063,2774,5020,5021,25189, 25190,22590, 22589,22774, 22773,5676, 5675,25191 & 
& , 25192,22594, 22593,22778, 22777,5684, 5683], & 
& edgecnc=[10023,8723,8815,266,10024,8725,8817,270], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2409),elname="xbrick",eltype="xbrick",typekey=2409) 

        call prepare(lib_xbrick(2410),key=2410, & 
& nodecnc=[1432,1422,102,2459,4004,3994,2674,5031,21682, 21681,10034, 10033,10066, 10065,23354, 23353 & 
& ,21686, 21685,10040, 10039,10074, 10073,23356, 23355], & 
& edgecnc=[8269,2445,2461,9105,8271,2448,2465,9106], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2410),elname="xbrick",eltype="xbrick",typekey=2410) 

        call prepare(lib_xbrick(2411),key=2411, & 
& nodecnc=[2454,2461,2429,865,5026,5033,5001,3437,8350, 8349,25170, 25169,17882, 17881,17330, 17329,8358 & 
& , 8357,25172, 25171,17888, 17887,17334, 17333], & 
& edgecnc=[1603,10013,6369,6093,1607,10014,6372,6095], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2411),elname="xbrick",eltype="xbrick",typekey=2411) 

        call prepare(lib_xbrick(2412),key=2412, & 
& nodecnc=[923,2435,859,906,3495,5007,3431,3478,18146, 18145,25182, 25181,17324, 17323,17978, 17977,18148 & 
& , 18147,25184, 25183,17328, 17327,17984, 17983], & 
& edgecnc=[6501,10019,6090,6417,6502,10020,6092,6420], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2412),elname="xbrick",eltype="xbrick",typekey=2412) 

        call prepare(lib_xbrick(2413),key=2413, & 
& nodecnc=[2466,2457,2419,910,5038,5029,4991,3482,18032, 18031,18262, 18261,18022, 18021,18002, 18001 & 
& ,18038, 18037,18266, 18265,18026, 18025,18008, 18007], & 
& edgecnc=[6444,6559,6439,6429,6447,6561,6441,6432], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2413),elname="xbrick",eltype="xbrick",typekey=2413) 

        call prepare(lib_xbrick(2414),key=2414, & 
& nodecnc=[805,773,812,2460,3377,3345,3384,5032,17132, 17131,16798, 16797,17180, 17179,12494, 12493,17136 & 
& , 17135,16804, 16803,17184, 17183,12502, 12501], & 
& edgecnc=[5994,5827,6018,3675,5996,5830,6020,3679], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2414),elname="xbrick",eltype="xbrick",typekey=2414) 

        call prepare(lib_xbrick(2415),key=2415, & 
& nodecnc=[2434,863,2480,2481,5006,3435,5052,5053,17498, 17497,17542, 17541,25193, 25194,25174, 25173 & 
& ,17506, 17505,17546, 17545,25195, 25196,25178, 25177], & 
& edgecnc=[6177,6199,10025,10015,6181,6201,10026,10017], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2415),elname="xbrick",eltype="xbrick",typekey=2415) 

        call prepare(lib_xbrick(2416),key=2416, & 
& nodecnc=[2474,1959,2467,2493,5046,4531,5039,5065,24326, 24325,24122, 24121,24450, 24449,9282, 9281,24328 & 
& , 24327,24126, 24125,24454, 24453,9288, 9287], & 
& edgecnc=[9591,9489,9653,2069,9592,9491,9655,2072], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2416),elname="xbrick",eltype="xbrick",typekey=2416) 

        call prepare(lib_xbrick(2417),key=2417, & 
& nodecnc=[2183,2439,2198,2165,4755,5011,4770,4737,25186, 25185,25064, 25063,24988, 24987,7052, 7051,25188 & 
& , 25187,25068, 25067,24994, 24993,7060, 7059], & 
& edgecnc=[10021,9960,9922,954,10022,9962,9925,958], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2417),elname="xbrick",eltype="xbrick",typekey=2417) 

        call prepare(lib_xbrick(2418),key=2418, & 
& nodecnc=[733,787,2402,2407,3305,3359,4974,4979,16402, 16401,16966, 16965,25162, 25161,15476, 15475,16408 & 
& , 16407,16970, 16969,25164, 25163,15484, 15483], & 
& edgecnc=[5629,5911,10009,5166,5632,5913,10010,5170], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2418),elname="xbrick",eltype="xbrick",typekey=2418) 

        call prepare(lib_xbrick(2419),key=2419, & 
& nodecnc=[785,844,2481,2480,3357,3416,5053,5052,12746, 12745,25176, 25175,25194, 25193,16510, 16509,12754 & 
& , 12753,25180, 25179,25196, 25195,16516, 16515], & 
& edgecnc=[3801,10016,10025,5683,3805,10018,10026,5686], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2419),elname="xbrick",eltype="xbrick",typekey=2419) 

        call prepare(lib_xbrick(2420),key=2420, & 
& nodecnc=[2508,2483,1222,1209,5080,5055,3794,3781,20218, 20217,12926, 12925,20208, 20207,20064, 20063 & 
& ,20220, 20219,12934, 12933,20214, 20213,20068, 20067], & 
& edgecnc=[7537,3891,7532,7460,7538,3895,7535,7462], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2420),elname="xbrick",eltype="xbrick",typekey=2420) 

        call prepare(lib_xbrick(2421),key=2421, & 
& nodecnc=[1348,146,2488,2486,3920,2718,5060,5058,21126, 21125,25197, 25198,25199, 25200,11368, 11367 & 
& ,21130, 21129,25201, 25202,25203, 25204,11374, 11373], & 
& edgecnc=[7991,10027,10028,3112,7993,10029,10030,3115], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2421),elname="xbrick",eltype="xbrick",typekey=2421) 

        call prepare(lib_xbrick(2422),key=2422, & 
& nodecnc=[146,1331,2506,2488,2718,3903,5078,5060,20904, 20903,20962, 20961,22476, 22475,25198, 25197 & 
& ,20908, 20907,20966, 20965,22484, 22483,25202, 25201], & 
& edgecnc=[7880,7909,8666,10027,7882,7911,8670,10029], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2422),elname="xbrick",eltype="xbrick",typekey=2422) 

        call prepare(lib_xbrick(2423),key=2423, & 
& nodecnc=[2525,2074,2490,2170,5097,4646,5062,4742,24914, 24913,23966, 23965,11810, 11809,25205, 25206 & 
& ,24916, 24915,23970, 23969,11816, 11815,25207, 25208], & 
& edgecnc=[9885,9411,3333,10031,9886,9413,3336,10032], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2423),elname="xbrick",eltype="xbrick",typekey=2423) 

        call prepare(lib_xbrick(2424),key=2424, & 
& nodecnc=[2514,1865,202,2491,5086,4437,2774,5063,21266, 21265,22910, 22909,25190, 25189,23534, 23533 & 
& ,21272, 21271,22912, 22911,25192, 25191,23540, 23539], & 
& edgecnc=[8061,8883,10023,9195,8064,8884,10024,9198], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2424),elname="xbrick",eltype="xbrick",typekey=2424) 

        call prepare(lib_xbrick(2425),key=2425, & 
& nodecnc=[2531,945,2495,965,5103,3517,5067,3537,17992, 17991,18334, 18333,18338, 18337,9694, 9693,17998 & 
& , 17997,18336, 18335,18340, 18339,9702, 9701], & 
& edgecnc=[6424,6595,6597,2275,6427,6596,6598,2279], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2425),elname="xbrick",eltype="xbrick",typekey=2425) 

        call prepare(lib_xbrick(2426),key=2426, & 
& nodecnc=[1265,1283,2524,2502,3837,3855,5096,5074,20490, 20489,20568, 20567,20854, 20853,20486, 20485 & 
& ,20494, 20493,20572, 20571,20856, 20855,20488, 20487], & 
& edgecnc=[7673,7712,7855,7671,7675,7714,7856,7672], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2426),elname="xbrick",eltype="xbrick",typekey=2426) 

        call prepare(lib_xbrick(2427),key=2427, & 
& nodecnc=[2130,2069,2155,2422,4702,4641,4727,4994,10592, 10591,24832, 24831,25166, 25165,24674, 24673 & 
& ,10598, 10597,24838, 24837,25168, 25167,24676, 24675], & 
& edgecnc=[2724,9844,10011,9765,2727,9847,10012,9766], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2427),elname="xbrick",eltype="xbrick",typekey=2427) 

        call prepare(lib_xbrick(2428),key=2428, & 
& nodecnc=[2456,825,853,2511,5028,3397,3425,5083,17154, 17153,16874, 16873,18042, 18041,17566, 17565,17158 & 
& , 17157,16878, 16877,18046, 18045,17570, 17569], & 
& edgecnc=[6005,5865,6449,6211,6007,5867,6451,6213], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2428),elname="xbrick",eltype="xbrick",typekey=2428) 

        call prepare(lib_xbrick(2429),key=2429, & 
& nodecnc=[903,856,893,2517,3475,3428,3465,5089,17394, 17393,17590, 17589,17576, 17575,17658, 17657,17400 & 
& , 17399,17594, 17593,17584, 17583,17666, 17665], & 
& edgecnc=[6125,6223,6216,6257,6128,6225,6220,6261], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2429),elname="xbrick",eltype="xbrick",typekey=2429) 

        call prepare(lib_xbrick(2430),key=2430, & 
& nodecnc=[1113,1087,1114,2521,3685,3659,3686,5093,19706, 19705,6492, 6491,19150, 19149,18986, 18985,19708 & 
& , 19707,6500, 6499,19152, 19151,18990, 18989], & 
& edgecnc=[7281,674,7003,6921,7282,678,7004,6923], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2430),elname="xbrick",eltype="xbrick",typekey=2430) 

        call prepare(lib_xbrick(2431),key=2431, & 
& nodecnc=[100,2487,2486,2488,2672,5059,5058,5060,9990, 9989,11370, 11369,25200, 25199,22474, 22473,9996 & 
& , 9995,11376, 11375,25204, 25203,22482, 22481], & 
& edgecnc=[2423,3113,10028,8665,2426,3116,10030,8669], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2431),elname="xbrick",eltype="xbrick",typekey=2431) 

        call prepare(lib_xbrick(2432),key=2432, & 
& nodecnc=[2516,1817,1962,2526,5088,4389,4534,5098,23126, 23125,12174, 12173,23398, 23397,24138, 24137 & 
& ,23132, 23131,12180, 12179,23404, 23403,24142, 24141], & 
& edgecnc=[8991,3515,9127,9497,8994,3518,9130,9499], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2432),elname="xbrick",eltype="xbrick",typekey=2432) 

        call prepare(lib_xbrick(2433),key=2433, & 
& nodecnc=[2529,2558,2559,2132,5101,5130,5131,4704,24852, 24851,25209, 25210,25211, 25212,21620, 21619 & 
& ,24856, 24855,25213, 25214,25215, 25216,21624, 21623], & 
& edgecnc=[9854,10033,10034,8238,9856,10035,10036,8240], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2433),elname="xbrick",eltype="xbrick",typekey=2433) 

        call prepare(lib_xbrick(2434),key=2434, & 
& nodecnc=[2553,163,2530,2550,5125,2735,5102,5122,11698, 11697,11550, 11549,21262, 21261,25217, 25218 & 
& ,11704, 11703,11556, 11555,21264, 21263,25219, 25220], & 
& edgecnc=[3277,3203,8059,10037,3280,3206,8060,10038], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2434),elname="xbrick",eltype="xbrick",typekey=2434) 

        call prepare(lib_xbrick(2435),key=2435, & 
& nodecnc=[1207,2557,2532,1220,3779,5129,5104,3792,20110, 20109,25221, 25222,20226, 20225,20290, 20289 & 
& ,20114, 20113,25223, 25224,20232, 20231,20292, 20291], & 
& edgecnc=[7483,10039,7541,7573,7485,10040,7544,7574], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2435),elname="xbrick",eltype="xbrick",typekey=2435) 

        call prepare(lib_xbrick(2436),key=2436, & 
& nodecnc=[1205,98,2532,2557,3777,2670,5104,5129,21402, 21401,20362, 20361,25222, 25221,20022, 20021,21404 & 
& , 21403,20364, 20363,25224, 25223,20028, 20027], & 
& edgecnc=[8129,7609,10039,7439,8130,7610,10040,7442], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2436),elname="xbrick",eltype="xbrick",typekey=2436) 

        call prepare(lib_xbrick(2437),key=2437, & 
& nodecnc=[2533,2064,2572,130,5105,4636,5144,2702,6758, 6757,25225, 25226,25227, 25228,24446, 24445,6766 & 
& , 6765,25229, 25230,25231, 25232,24448, 24447], & 
& edgecnc=[807,10041,10042,9651,811,10043,10044,9652], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2437),elname="xbrick",eltype="xbrick",typekey=2437) 

        call prepare(lib_xbrick(2438),key=2438, & 
& nodecnc=[2138,167,2551,2564,4710,2739,5123,5136,24806, 24805,24534, 24533,24778, 24777,25233, 25234 & 
& ,24808, 24807,24538, 24537,24784, 24783,25235, 25236], & 
& edgecnc=[9831,9695,9817,10045,9832,9697,9820,10046], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2438),elname="xbrick",eltype="xbrick",typekey=2438) 

        call prepare(lib_xbrick(2439),key=2439, & 
& nodecnc=[2525,2170,2542,2541,5097,4742,5114,5113,25206, 25205,25020, 25019,25010, 25009,7992, 7991,25208 & 
& , 25207,25024, 25023,25014, 25013,8000, 7999], & 
& edgecnc=[10031,9938,9933,1424,10032,9940,9935,1428], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2439),elname="xbrick",eltype="xbrick",typekey=2439) 

        call prepare(lib_xbrick(2440),key=2440, & 
& nodecnc=[1091,1045,121,2543,3663,3617,2693,5115,19044, 19043,18720, 18719,18730, 18729,19326, 19325 & 
& ,19050, 19049,18724, 18723,18738, 18737,19332, 19331], & 
& edgecnc=[6950,6788,6793,7091,6953,6790,6797,7094], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2440),elname="xbrick",eltype="xbrick",typekey=2440) 

        call prepare(lib_xbrick(2441),key=2441, & 
& nodecnc=[893,815,2469,2527,3465,3387,5041,5099,17592, 17591,16572, 16571,17342, 17341,25237, 25238,17596 & 
& , 17595,16580, 16579,17350, 17349,25239, 25240], & 
& edgecnc=[6224,5714,6099,10047,6226,5718,6103,10048], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2441),elname="xbrick",eltype="xbrick",typekey=2441) 

        call prepare(lib_xbrick(2442),key=2442, & 
& nodecnc=[893,2527,914,2544,3465,5099,3486,5116,25238, 25237,18062, 18061,18394, 18393,17578, 17577,25240 & 
& , 25239,18064, 18063,18400, 18399,17586, 17585], & 
& edgecnc=[10047,6459,6625,6217,10048,6460,6628,6221], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2442),elname="xbrick",eltype="xbrick",typekey=2442) 

        call prepare(lib_xbrick(2443),key=2443, & 
& nodecnc=[2545,1358,2568,100,5117,3930,5140,2672,20990, 20989,25241, 25242,21056, 21055,22472, 22471 & 
& ,20996, 20995,25243, 25244,21062, 21061,22480, 22479], & 
& edgecnc=[7923,10049,7956,8664,7926,10050,7959,8668], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2443),elname="xbrick",eltype="xbrick",typekey=2443) 

        call prepare(lib_xbrick(2444),key=2444, & 
& nodecnc=[2546,2545,2506,1338,5118,5117,5078,3910,20992, 20991,22470, 22469,20964, 20963,21010, 21009 & 
& ,20998, 20997,22478, 22477,20968, 20967,21012, 21011], & 
& edgecnc=[7924,8663,7910,7933,7927,8667,7912,7934], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2444),elname="xbrick",eltype="xbrick",typekey=2444) 

        call prepare(lib_xbrick(2445),key=2445, & 
& nodecnc=[986,1002,963,2549,3558,3574,3535,5121,11834, 11833,18468, 18467,18406, 18405,18054, 18053,11842 & 
& , 11841,18474, 18473,18408, 18407,18060, 18059], & 
& edgecnc=[3345,6662,6631,6455,3349,6665,6632,6458], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2445),elname="xbrick",eltype="xbrick",typekey=2445) 

        call prepare(lib_xbrick(2446),key=2446, & 
& nodecnc=[165,2570,2559,2558,2737,5142,5131,5130,23880, 23879,25245, 25246,25210, 25209,24850, 24849 & 
& ,23884, 23883,25247, 25248,25214, 25213,24854, 24853], & 
& edgecnc=[9368,10051,10033,9853,9370,10052,10035,9855], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2446),elname="xbrick",eltype="xbrick",typekey=2446) 

        call prepare(lib_xbrick(2447),key=2447, & 
& nodecnc=[117,2156,2548,2554,2689,4728,5120,5126,24318, 24317,24842, 24841,24826, 24825,24814, 24813 & 
& ,24322, 24321,24846, 24845,24828, 24827,24816, 24815], & 
& edgecnc=[9587,9849,9841,9835,9589,9851,9842,9836], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2447),elname="xbrick",eltype="xbrick",typekey=2447) 

        call prepare(lib_xbrick(2448),key=2448, & 
& nodecnc=[2571,2553,2550,1709,5143,5125,5122,4281,22506, 22505,25218, 25217,21016, 21015,7188, 7187,22508 & 
& , 22507,25220, 25219,21022, 21021,7196, 7195], & 
& edgecnc=[8681,10037,7936,1022,8682,10038,7939,1026], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2448),elname="xbrick",eltype="xbrick",typekey=2448) 

        call prepare(lib_xbrick(2449),key=2449, & 
& nodecnc=[2064,2552,2132,2559,4636,5124,4704,5131,6764, 6763,24858, 24857,25212, 25211,25249, 25250,6772 & 
& , 6771,24860, 24859,25216, 25215,25251, 25252], & 
& edgecnc=[810,9857,10034,10053,814,9858,10036,10054], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2449),elname="xbrick",eltype="xbrick",typekey=2449) 

        call prepare(lib_xbrick(2450),key=2450, & 
& nodecnc=[1102,1143,2560,1128,3674,3715,5132,3700,19272, 19271,19890, 19889,20498, 20497,11482, 11481 & 
& ,19276, 19275,19894, 19893,20500, 20499,11486, 11485], & 
& edgecnc=[7064,7373,7677,3169,7066,7375,7678,3171], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2450),elname="xbrick",eltype="xbrick",typekey=2450) 

        call prepare(lib_xbrick(2451),key=2451, & 
& nodecnc=[953,2537,1015,993,3525,5109,3587,3565,18386, 18385,25253, 25254,18666, 18665,18546, 18545,18390 & 
& , 18389,25255, 25256,18672, 18671,18548, 18547], & 
& edgecnc=[6621,10055,6761,6701,6623,10056,6764,6702], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2451),elname="xbrick",eltype="xbrick",typekey=2451) 

        call prepare(lib_xbrick(2452),key=2452, & 
& nodecnc=[1015,2537,980,938,3587,5109,3552,3510,25254, 25253,18388, 18387,18100, 18099,18462, 18461,25256 & 
& , 25255,18392, 18391,18104, 18103,18464, 18463], & 
& edgecnc=[10055,6622,6478,6659,10056,6624,6480,6660], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2452),elname="xbrick",eltype="xbrick",typekey=2452) 

        call prepare(lib_xbrick(2453),key=2453, & 
& nodecnc=[1041,2561,2556,1044,3613,5133,5128,3616,18662, 18661,18080, 18079,19038, 19037,19032, 19031 & 
& ,18668, 18667,18088, 18087,19040, 19039,19036, 19035], & 
& edgecnc=[6759,6468,6947,6944,6762,6472,6948,6946], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2453),elname="xbrick",eltype="xbrick",typekey=2453) 

        call prepare(lib_xbrick(2454),key=2454, & 
& nodecnc=[2138,2564,2542,2179,4710,5136,5114,4751,25234, 25233,25012, 25011,25018, 25017,24894, 24893 & 
& ,25236, 25235,25016, 25015,25022, 25021,24898, 24897], & 
& edgecnc=[10045,9934,9937,9875,10046,9936,9939,9877], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2454),elname="xbrick",eltype="xbrick",typekey=2454) 

        call prepare(lib_xbrick(2455),key=2455, & 
& nodecnc=[2568,1358,2566,2174,5140,3930,5138,4746,25242, 25241,21068, 21067,23920, 23919,21058, 21057 & 
& ,25244, 25243,21074, 21073,23924, 23923,21064, 21063], & 
& edgecnc=[10049,7962,9388,7957,10050,7965,9390,7960], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2455),elname="xbrick",eltype="xbrick",typekey=2455) 

        call prepare(lib_xbrick(2456),key=2456, & 
& nodecnc=[2064,2559,2570,2572,4636,5131,5142,5144,25250, 25249,25246, 25245,25257, 25258,25226, 25225 & 
& ,25252, 25251,25248, 25247,25259, 25260,25230, 25229], & 
& edgecnc=[10053,10051,10057,10041,10054,10052,10058,10043], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2456),elname="xbrick",eltype="xbrick",typekey=2456) 

        call prepare(lib_xbrick(2457),key=2457, & 
& nodecnc=[2570,1974,130,2572,5142,4546,2702,5144,23878, 23877,23542, 23541,25228, 25227,25258, 25257 & 
& ,23882, 23881,23548, 23547,25232, 25231,25260, 25259], & 
& edgecnc=[9367,9199,10042,10057,9369,9202,10044,10058], & 
& bulkmat=1, cohmat=5 ) 
        call update(lib_elem(2457),elname="xbrick",eltype="xbrick",typekey=2457) 

    end subroutine initialize_lib_elem        
