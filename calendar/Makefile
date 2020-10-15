calendar: gitsearch.ml calendar.ml
	ocamlc -g -o calendar str.cma unix.cma gitsearch.ml calendar.ml

yr2018: calendar
	./calendar --start v4.15 --end v4.16 --out yr2018a --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.16 --end v4.17 --out yr2018b --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.17 --end v4.18 --out yr2018c --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.18 --end v4.19 --out yr2018d --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.19 --end v4.20 --out yr2018e --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux

yr2017: calendar
	./calendar --start v4.14 --end v4.15 --out yr2017a --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.13 --end v4.14 --out yr2017b --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.12 --end v4.13 --out yr2017c --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.11 --end v4.12 --out yr2017d --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux
	./calendar --start v4.10 --end v4.11 --out yr2017e --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux

yr2017b_redo: calendar
	./calendar --start v4.13 --end v4.14 --out yr2017b_redo --args "--minf 10" \
	--update "cd linux; git pull" /home/julia/linux

v19: calendar
	./calendar --start v4.18 --end v4.19 --out v4.19 \
	--update "cd linux; git pull" /home/julia/linux

v18: calendar
	./calendar --start v4.17 --end v4.18 --out v4.18 \
	--update "cd linux; git pull" /home/julia/linux

v17: calendar
	./calendar --start v4.16 --end v4.17 --out v4.17 \
	--update "cd linux; git pull" /home/julia/linux

v16: calendar
	./calendar --start v4.15 --end v4.16 --out v4.16 \
	--update "cd linux; git pull" /home/julia/linux

stables: calendar
	./calendar --start v4.10-rc1 --end v4.10.17 --out v4.10 \
	--update "cd linux-stable; git pull" /home/julia/linux-stable
	./calendar --start v4.11-rc1 --end v4.11.12 --out v4.11 \
	--update "cd linux-stable; git pull" /home/julia/linux-stable
	./calendar --start v4.12-rc1 --end v4.12.14 --out v4.12 \
	--update "cd linux-stable; git pull" /home/julia/linux-stable
	./calendar --start v4.13-rc1 --end v4.13.12 --out v4.13 \
	--update "cd linux-stable; git pull" /home/julia/linux-stable

veryrecent: calendar
	./calendar --start 100116 --end 093017 \
	--out sep2017_linux --inc 5 --update "cd linux; git pull" \
	/home/julia/linux

october: calendar
	./calendar --start 100116 --end 103116 \
	--out sep2017_linux --inc 5 --update "cd linux; git pull" \
	/home/julia/linux

recent: calendar
	./calendar --start 100115 --end 123115 \
	--out dec2015_linux --inc 2 --update "cd linux.git; git pull" \
	/home/julia/linux.git

staging: calendar
	./calendar --start 080115 --end 013116 \
	--out staging_linux --inc 4 --update "cd linux.git; git pull" \
	--restrict drivers/staging /home/julia/linux.git

pdfs:
	pdftk android_frameworks/*/*pdf cat output andfrm.pdf

lpdfs:
	pdftk sep2017_linux/*/*pdf cat output linux17.pdf

gleixcons: calendar
	./calendar /home/julia/linux-next --gitsearch \
	"--author=\"Thomas Gleixner\" --grep \"Consolidate chained IRQ\"" \
	--out $@ --update "echo nothing"

julia_add_onp: calendar
	./calendar /home/julia/linux-next --gitsearch \
	"--author=\"Julia Lawall\" --grep \"missing of_node_put\"" \
	--out $@ --update "echo nothing"

kees_timer1: calendar
	./calendar /home/julia/linux-next --gitsearch \
	"--author=\"Kees Cook\" --grep \"init_timer() -> setup_timer()\"" \
	--out $@ --update "echo nothing"

kees_timer2: calendar
	./calendar /home/julia/linux-next --gitsearch \
	"--author=\"Kees Cook\" --grep \"setup_timer() -> timer_setup()\"" \
	--out $@ --update "echo nothing"

lynxpoint: calendar
	./calendar /home/julia/linux-next --prequel \
	/home/julia/incoming/drivers_gpio_gpio-lynxpoint.c_1d09aaa \
	--update "echo nothing"

light: calendar
	./calendar /home/julia/linux-next --prequel \
	/home/julia/incoming/drivers_iio_light_cm36651.c_e590d45 \
	--update "echo nothing"

zephyr: calendar
	./calendar --start 041015 --end 070618 --out zephyr --inc 10 \
	--update "cd /home/julia/zephyr; git pull" /home/julia/zephyr
