struct Point {
  int x;
  int y;
};

typedef struct Point Point;

Point* neighbors(int x, int y, int dir_x, int dir_y);
