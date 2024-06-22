# コンパイラ
FC = ifort

# コンパイルフラグ
FFLAGS = -O3 -qopenmp -mkl=parallel -I$(MOD_DIR)

# ターゲット実行ファイル名
TARGET = main

# モジュールディレクトリ
MOD_DIR = modules

# ソースファイル
MOD_SRC = $(MOD_DIR)/decision_tree_types.f90 $(MOD_DIR)/decision_tree_utils.f90 $(MOD_DIR)/decision_tree_split.f90 $(MOD_DIR)/decision_tree_metrics.f90 $(MOD_DIR)/decision_tree_build.f90 $(MOD_DIR)/decision_tree_io.f90
MAIN_SRC = main.f90

# オブジェクトファイル
MOD_OBJ = $(MOD_SRC:.f90=.o)
MAIN_OBJ = $(MAIN_SRC:.f90=.o)

# ルール定義
all: clean $(TARGET)

# ターゲットファイルの生成
$(TARGET): $(MOD_OBJ) $(MAIN_OBJ)
	$(FC) $(FFLAGS) -o $@ $(MOD_OBJ) $(MAIN_OBJ)

# モジュールファイルのコンパイル
$(MOD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(FC) $(FFLAGS) -c $< -o $@

# メインファイルのコンパイル
main.o: main.f90 $(MOD_OBJ)
	$(FC) $(FFLAGS) -c main.f90 -o main.o

# クリーンアップ
clean:
	rm -f $(MOD_OBJ) $(MAIN_OBJ) $(TARGET)
	rm -f $(MOD_DIR)/*.mod

# 実行
run: $(TARGET)
	./$(TARGET)

.PHONY: all clean run
