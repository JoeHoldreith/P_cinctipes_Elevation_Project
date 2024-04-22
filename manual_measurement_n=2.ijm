function yolkarea(input, filename) {
open(input + filename);

run("RGB Color");

// Color Thresholder 2.14.0/1.54f
// Autogenerated macro, single images only!
min=newArray(3);
max=newArray(3);
filter=newArray(3);
a=getTitle();
run("RGB Stack");
run("Convert Stack to Images");
selectWindow("Red");
rename("0");
selectWindow("Green");
rename("1");
selectWindow("Blue");
rename("2");
min[0]=103;
max[0]=255;
filter[0]="pass";
min[1]=72;
max[1]=255;
filter[1]="stop";
min[2]=89;
max[2]=255;
filter[2]="stop";
for (i=0;i<3;i++){
  selectWindow(""+i);
  setThreshold(min[i], max[i]);
  run("Convert to Mask");
  if (filter[i]=="stop")  run("Invert");
}
imageCalculator("AND create", "0","1");
imageCalculator("AND create", "Result of 0","2");
for (i=0;i<3;i++){
  selectWindow(""+i);
  close();
}
selectWindow("Result of 0");
close();
selectWindow("Result of Result of 0");
rename(a);
// Colour Thresholding-------------
run("Create Selection");
run("Measure");
close();
close("*");



}

function embryoarea(input, filename) {
open(input + filename);
run("RGB Color");
setOption("BlackBackground", true);
run("Convert to Mask");
run("Fill Holes");
run("Ellipse Split", "binary=[Use standard watershed] add_to_manager merge_when_relativ_overlap_larger_than_threshold overlap=95 major=1000-Infinity minor=1000-Infinity aspect=1-Infinity");
roiManager("Measure");
close();
selectWindow("ROI Manager");
run("Close");
close("*");

}

Master = "C:/Users/joemh/Desktop/1_12EXP_UNPROCESSED_TIFS/Unprocessed/manual_measuring/2_8_HS2_HZ7/";
dummy = getFileList(Master);

embryoarea(Master, dummy[0]);
embryoarea(Master, dummy[1]);


saveAs("Results", File.directory + "embryo_area.csv");
selectWindow("Results");
run("Close");

yolkarea(Master, dummy[0]);
yolkarea(Master, dummy[1]);


saveAs("Results", File.directory + "yolk_area.csv");
selectWindow("Results");
run("Close");
		