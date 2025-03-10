package tests

import (
    "testing"
)

// TestFalso verifica que 2 + 2 sea igual a 4.
func TestFalso(t *testing.T) {
    resultado := 2 + 2
    esperado := 4

    if resultado != esperado {
        t.Errorf("Test fallo: se esperaba %d, pero se obtuvo %d", esperado, resultado)
    } else {
        t.Logf("Test exitoso: se obtuvo %d, que es igual a %d", resultado, esperado)
    }
}