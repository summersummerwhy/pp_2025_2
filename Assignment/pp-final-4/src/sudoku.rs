use crate::grid::*;

pub fn solve(grid: &mut [[u8; SIZE]; SIZE]) -> bool {
    // su 내부 recursive 함수
    // su (board, index, num)
    // 1. if (index < 0) false
    // 2. else if (index > 15) true
    // 3. else if (board[index] != 0)
        // 3-1. if (valid_num(board, row, col, num_now)) su(board, index+1, 1)
        // 3-2. else false
    // 4. else if (num > 4) write 0; false;
    // 5. else
    //    write num
    //    (1) if (valid_num(board, index))
    //       (2-1) if (su(board, index+1, 1) true
    //       (2-2) else write 0; su(board, index, num+1);
    //    (2) else write 0; su(board, index, num+1);
    // 지금 내가 쓸 권한을 계속 넘겨주는 구조가 &mut로는 안됨 -> 쓰기까지 해야되니까 RefCell 사용해야됨!

    fn solve_recursive(board: &mut [[u8; SIZE]; SIZE], index: usize, num_now: u8) -> bool {
        if index >= SIZE * SIZE {print_grid(board); true}
        else  {
            let row: usize = index / 4;
            let col: usize = index - row * 4;
            if board[row][col] != 0 {
                let num_written = board[row][col];
                if valid_num(&board, row, col, num_written) {return solve_recursive(board, index + 1, 1)}
                else {return false}
            }
            else if num_now > SIZE as u8 {return false}
            else {
                board[row][col] = num_now;
                if valid_num(&board, row, col, num_now) {
                    if solve_recursive(board, index + 1, 1) {return true}
                    else {
                        board[row][col] = 0;
                        return solve_recursive(board, index, num_now + 1)
                    }
                }
                else {
                    board[row][col] = 0;
                    return solve_recursive(board, index, num_now + 1)
                }
            }
        }
    }

    return solve_recursive(grid, 0, 1)
}

fn valid_num(grid: &[[u8; SIZE]; SIZE], row_idx: usize, col_idx: usize, num_now: u8) -> bool {

    // Validate that row is correct
    for row in 0..SIZE {
        let num_written = grid[row][col_idx];
        if row != row_idx && num_written == num_now {return false}
    }

    // Validate that col is correct
    for col in 0..SIZE {
        let num_written = grid[row_idx][col];
        if col != col_idx && num_written == num_now {return false}
    }

    // Validate subgrids
    let start_row = (row_idx / 2) * 2;
    let start_col = (col_idx / 2) * 2;

    for row in start_row..start_row + SUBGRID_SIZE {
        for col in start_col..start_col + SUBGRID_SIZE {
            if row != row_idx && col != col_idx && grid[row][col]== num_now {return false}
        }
    }

    return true

}

// 문제: 지금 board를 계속 읽고, 거기다 써가면서 문제를 풀어야되는데

#[cfg(test)]
mod tests {
    use super::*;

    fn validate_solved_grid(grid: &[[u8; SIZE]; SIZE]) {
        // Validate that the solved grid is correct
        for row in 0..SIZE {
            let mut row_set = [false; SIZE + 1];
            let mut col_set = [false; SIZE + 1];
            for col in 0..SIZE {
                row_set[grid[row][col] as usize] = true;
                col_set[grid[col][row] as usize] = true;
            }
            assert!(row_set[1..].iter().all(|&x| x));
            assert!(col_set[1..].iter().all(|&x| x));
        }

        // Validate subgrids
        for start_row in (0..SIZE).step_by(SUBGRID_SIZE) {
            for start_col in (0..SIZE).step_by(SUBGRID_SIZE) {
                let mut subgrid_set = [false; SIZE + 1];
                for row in 0..SUBGRID_SIZE {
                    for col in 0..SUBGRID_SIZE {
                        subgrid_set[grid[start_row + row][start_col + col] as usize] = true;
                    }
                }
                assert!(subgrid_set[1..].iter().all(|&x| x));
            }
        }
    }

    #[test]
    fn test_solver_with_empty_grid() {
        let mut grid = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]];

        assert!(solve(&mut grid));

        // Validate that the solved grid is correct
        validate_solved_grid(&grid);
    }

    #[test]
    fn test_solver_with_partial_grid() {
        let mut grid = [[1, 0, 3, 4], [3, 0, 0, 0], [0, 0, 4, 0], [4, 0, 0, 0]];

        assert!(solve(&mut grid));

        // Validate that the grid is solved
        validate_solved_grid(&grid);
    }

    #[test]
    fn test_solver_with_fully_solved_grid() {
        let mut grid = [[1, 2, 3, 4], [3, 4, 1, 2], [2, 1, 4, 3], [4, 3, 2, 1]];

        assert!(solve(&mut grid));

        // Ensure the grid remains unchanged
        let expected = [[1, 2, 3, 4], [3, 4, 1, 2], [2, 1, 4, 3], [4, 3, 2, 1]];
        assert_eq!(grid, expected);
    }

    #[test]
    fn test_solver_with_unsolvable_grid() {
        let mut grid = [
            [1, 2, 3, 4],
            [3, 4, 1, 2],
            [2, 1, 4, 3],
            [4, 3, 2, 3], // Invalid grid: duplicate '3' in the last row
        ];

        assert!(!solve(&mut grid)); // Should return false
    }
}
