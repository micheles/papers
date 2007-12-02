dot hello.dot -Tps -o fig1.ps
dot hello2.dot -Tps -o fig2.ps
dot simple_hierarchy.dot -Tps -o fig3.ps
dot revolution.dot -Tps -o fig4.ps
dot hierarchy.dot -Tps -o fig5.ps
dot hierarchy_of_D.dot -Tps -o fig6.ps

zip dot dot.txt dot.html default.css fig1.*  fig2.*  fig3.*  fig4.*  fig5.*  fig6.*  fig6.* 
